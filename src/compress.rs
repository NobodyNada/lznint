use std::collections::HashMap;

use crate::{Command, Reference};

/// Compresses the provided data.
pub fn compress(src: &[u8]) -> Vec<u8> {
    let mut dst = Vec::new();

    let mut cache = BackreferenceCache::default();

    let mut i = 0;
    let mut prev_copy = Vec::new();
    while i < src.len() {
        let best = find_best(src, i, &mut cache);
        // The new command has to save at least 2 bytes to be worthwhile over a copy
        if best.len() >= best.cost() + 2 {
            if !prev_copy.is_empty() {
                Command::Copy(&prev_copy[..]).write(&mut dst);
                prev_copy.clear();
            }
            best.write(&mut dst);
            cache.fill(src, i, i + best.len());
            i += best.len();
        } else {
            prev_copy.push(src[i]);
            cache.fill(src, i, i + 1);
            i += 1;

            if prev_copy.len() == Command::MAX_LEN {
                Command::Copy(&prev_copy[..]).write(&mut dst);
                prev_copy.clear();
            }
        }
    }

    if !prev_copy.is_empty() {
        Command::Copy(&prev_copy[..]).write(&mut dst);
    }

    Command::Stop.write(&mut dst);

    dst
}

fn find_best<'a>(src: &'a [u8], i: usize, cache: &mut BackreferenceCache) -> Command<'a> {
    let mut candidates = [
        // byte fill
        Some(Command::ByteFill {
            data: src[i],
            len: std::cmp::min(
                src[i..].iter().take_while(|&&x| x == src[i]).count(),
                Command::MAX_LEN,
            ),
        }),
        // word fill
        if src.len() - i < 2 {
            None
        } else {
            let word = u16::from_le_bytes([src[i], src[i + 1]]);
            let mut len = src[i..]
                .chunks_exact(2)
                .take_while(|c| u16::from_le_bytes((*c).try_into().unwrap()) == word)
                .count()
                * 2;

            // A word fill can have a partial last word
            if src.get(i + len).copied() == Some(word as u8) {
                len += 1;
            }

            let len = std::cmp::min(len, Command::MAX_LEN);
            Some(Command::WordFill { data: word, len })
        },
        // incrementing
        Some(Command::Incrementing {
            start: src[i],
            len: std::cmp::min(
                std::iter::zip(
                    std::iter::successors(Some(src[i]), |x| Some(x.wrapping_add(1))),
                    src[i..].iter().copied(),
                )
                .take_while(|(a, b)| a == b)
                .count(),
                Command::MAX_LEN,
            ),
        }),
    ];
    // We want to prioritize earlier candidates in case of ties, but max_by prioritizes last.
    // So reverse the order:
    candidates.reverse();

    let best_non_backreference = candidates
        .into_iter()
        .flatten()
        .max_by(|a, b| a.bytes_per_cost().total_cmp(&b.bytes_per_cost()))
        .unwrap();

    // If we can get a max-length command without needing a backreference,
    // skip the expensive backreference search.
    if best_non_backreference.len() == Command::MAX_LEN {
        return best_non_backreference;
    }

    let best_backreference = cache.lookup(src, i);
    if best_backreference
        .as_ref()
        .is_some_and(|b| b.bytes_per_cost() > best_non_backreference.bytes_per_cost())
    {
        best_backreference.unwrap()
    } else {
        best_non_backreference
    }
}

impl Command<'_> {
    fn len(&self) -> usize {
        match self {
            Command::Copy(buf) => buf.len(),
            Command::ByteFill { data: _, len } => *len,
            Command::WordFill { data: _, len } => *len,
            Command::Incrementing { start: _, len } => *len,
            Command::Backreference {
                src: _,
                invert: _,
                len,
            } => *len,
            Command::Stop => 0,
        }
    }

    fn cost(&self) -> usize {
        let args = match self {
            Command::Copy(buf) => buf.len(),
            Command::ByteFill { data: _, len: _ } => 1,
            Command::WordFill { data: _, len: _ } => 2,
            Command::Incrementing { start: _, len: _ } => 1,
            Command::Backreference {
                src: Reference::Relative(_),
                invert: true,
                len,
            } if *len <= 32 => 2,
            Command::Backreference {
                src: Reference::Relative(_),
                invert: _,
                len: _,
            } => 1,
            Command::Backreference {
                src: _,
                invert: _,
                len: _,
            } => 2,
            Command::Stop => 0,
        };

        if self.len() <= 32 {
            args + 1
        } else {
            args + 2
        }
    }

    fn bytes_per_cost(&self) -> f32 {
        self.len() as f32 / self.cost() as f32
    }

    fn write(&self, dst: &mut Vec<u8>) {
        fn _write(cmd: u8, len: usize, data: &[u8], dst: &mut Vec<u8>) {
            let len = len - 1;
            if len < 32 && cmd != 7 {
                dst.push((cmd << 5) | len as u8);
            } else {
                assert!(len < Command::MAX_LEN);
                dst.push(0xE0 | (cmd << 2) | (len >> 8) as u8);
                dst.push(len as u8);
            }

            dst.extend_from_slice(data);
        }
        match self {
            Command::Copy(data) => _write(0, self.len(), data, dst),
            Command::ByteFill { data, len } => _write(1, *len, &[*data], dst),
            Command::WordFill { data, len } => _write(2, *len, &data.to_le_bytes(), dst),
            Command::Incrementing { start, len } => _write(3, *len, &[*start], dst),
            Command::Backreference { src, invert, len } => {
                match src {
                    Reference::Absolute(addr) => {
                        _write(4 | *invert as u8, *len, &addr.to_le_bytes(), dst)
                    }
                    Reference::Relative(offset) => {
                        assert_ne!(*offset, 0);
                        if *invert {
                            assert!(*len <= 0x300);
                        }
                        _write(6 | *invert as u8, *len, &[*offset], dst)
                    }
                };
            }

            Command::Stop => dst.push(0xFF),
        };
    }
}

impl Reference {
    fn try_relative(src: usize, dst: usize) -> Option<Reference> {
        let distance = u8::try_from(dst - src).ok()?;
        Some(Reference::Relative(distance))
    }

    fn try_absolute(src: usize) -> Option<Reference> {
        Some(Reference::Absolute(u16::try_from(src).ok()?))
    }
}

const PREFIX_LEN: usize = 3;
#[derive(Default, Debug)]
/// A cache mapping prefixes to offsets where we've seen that prefix.
struct BackreferenceCache(HashMap<[u8; PREFIX_LEN], Vec<usize>>);

impl BackreferenceCache {
    /// Searches for a backreference that matches the destination position.
    fn lookup<'a>(&mut self, data: &'a [u8], dst: usize) -> Option<Command<'a>> {
        if dst > (data.len() - PREFIX_LEN) {
            return None;
        }

        let non_inv = self.lookup_inv(data, dst, false);
        let inv = self.lookup_inv(data, dst, true);

        [non_inv, inv]
            .into_iter()
            .flatten()
            .max_by(|a, b| a.bytes_per_cost().total_cmp(&b.bytes_per_cost()))
    }

    fn fill(&mut self, data: &[u8], from: usize, to: usize) {
        for i in from..(to.min(data.len() - PREFIX_LEN)) {
            let prefix: [u8; PREFIX_LEN] = data[i..(i + PREFIX_LEN)].try_into().unwrap();
            self.0.entry(prefix).or_default().push(i);
        }
    }

    fn lookup_inv<'a>(&self, data: &'a [u8], dst: usize, invert: bool) -> Option<Command<'a>> {
        // Find everywhere we've seen this prefix before.
        let prefix = <[u8; PREFIX_LEN]>::try_from(&data[dst..(dst + PREFIX_LEN)])
            .unwrap()
            .map(|b| if invert { !b } else { b });

        let matches = self.0.get(&prefix);
        let matches = matches
            .iter()
            .flat_map(|v| v.iter())
            .flat_map(|&src| Self::get_match(data, src, dst, invert));

        let mut best = None;
        // Find the best match.
        for m in matches {
            // Bail out early if we find a max-length backreference.
            if m.len() == Command::MAX_LEN {
                return Some(m);
            }

            if best
                .as_ref()
                .is_none_or(|b: &Command| m.bytes_per_cost() > b.bytes_per_cost())
            {
                best = Some(m);
            }
        }
        best
    }

    fn get_match<'a>(data: &'a [u8], src: usize, dst: usize, invert: bool) -> Option<Command<'a>> {
        debug_assert!(data[dst..(dst + PREFIX_LEN)]
            .iter()
            .copied()
            .eq(data[src..(src + PREFIX_LEN)]
                .iter()
                .map(|b| if invert { !b } else { *b })));

        let mut len = std::iter::zip(
            data[dst + 3..].iter().copied(),
            data[src + 3..].iter().copied(),
        )
        .take_while(|(a, b)| *a == if invert { !b } else { *b })
        .take(Command::MAX_LEN - PREFIX_LEN)
        .count()
            + PREFIX_LEN;

        let reference = if let Some(relative) = Reference::try_relative(src, dst) {
            if invert && len > 0x300 {
                // A relative inverted match has a shorter max length
                // due to collision with the stop command.
                // If we hit that limit, try encoding as absolute.
                if let Some(absolute) = Reference::try_absolute(src) {
                    Some(absolute)
                } else {
                    // If that fails, just truncate.
                    len = 0x300;
                    Some(relative)
                }
            } else {
                Some(relative)
            }
        } else {
            Reference::try_absolute(src)
        };

        Some(Command::Backreference {
            src: reference?,
            invert,
            len,
        })
    }
}
