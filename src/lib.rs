//! A library for compressing and decompressing data using the LZ-based compression format used by
//! Nintendo in Super Metroid.
//!
//! ## Example
//! ```
//! let input = [0x1, 0x2, 0x3, 0x4, 0x1, 0x2, 0x3, 0x4];
//!
//! let compressed = lznint::compress(&input);
//! println!("{:x?}", compressed);  // [63, 1, c3, 4, ff]
//!
//! let decompressed = lznint::decompress(&compressed).expect("Decompressino failed");
//! assert_eq!(&decompressed, &input);
//! ```

mod compress;
mod decompress;

pub use compress::compress;
pub use decompress::{decompress, DecompressionError};

#[derive(Debug)]
enum Command<'a> {
    Copy(&'a [u8]),
    ByteFill {
        data: u8,
        len: usize,
    },
    WordFill {
        data: u16,
        len: usize,
    },
    Incrementing {
        start: u8,
        len: usize,
    },
    Backreference {
        src: Reference,
        invert: bool,
        len: usize,
    },
    Stop,
}

impl Command<'_> {
    const MAX_LEN: usize = 0x400;
}

#[derive(Debug)]
enum Reference {
    Absolute(u16),
    Relative(u8),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_decompress() {
        assert_eq!(decompress(&[0x3, 1, 2, 3, 4, 0xFF]), Ok(vec![1, 2, 3, 4]));
        assert_eq!(
            decompress(&[0x23, 0xAA, 0xFF]),
            Ok(vec![0xAA, 0xAA, 0xAA, 0xAA])
        );
        assert_eq!(
            decompress(&[0x43, 0xAA, 0x55, 0xFF]),
            Ok(vec![0xAA, 0x55, 0xAA, 0x55])
        );
        assert_eq!(decompress(&[0x63, 1, 0xFF]), Ok(vec![1, 2, 3, 4]));

        assert_eq!(
            decompress(&[0x2, 1, 2, 3, 0x85, 0x00, 0x00, 0xFF]),
            Ok(vec![1, 2, 3, 1, 2, 3, 1, 2, 3])
        );
        assert_eq!(
            decompress(&[0x2, 1, 2, 3, 0xA5, 0x00, 0x00, 0xFF]),
            Ok(vec![1, 2, 3, !1, !2, !3, 1, 2, 3])
        );
        assert_eq!(
            decompress(&[0x2, 1, 2, 3, 0xC5, 0x03, 0xFF]),
            Ok(vec![1, 2, 3, 1, 2, 3, 1, 2, 3])
        );
        assert_eq!(
            decompress(&[0x2, 1, 2, 3, 0xFC, 0x5, 0x03, 0xFF]),
            Ok(vec![1, 2, 3, !1, !2, !3, 1, 2, 3])
        );

        assert_eq!(
            decompress(&[0x2, 1, 2, 3, 0xFB, 0xFE, 0x3, 0xFF]),
            Ok([1, 2, 3].into_iter().cycle().take(1026).collect())
        );
    }

    #[test]
    fn test_compress() {
        assert_eq!(compress(&[0, 2, 4, 6]), vec![0x03, 0, 2, 4, 6, 0xFF]);
        assert_eq!(compress(&[1, 1, 1, 1]), vec![0x23, 1, 0xFF]);
        assert_eq!(compress(&[1, 2, 1, 2, 1, 2]), vec![0x45, 1, 2, 0xFF]);
        assert_eq!(compress(&[1, 2, 1, 2, 1]), vec![0x44, 1, 2, 0xFF]);
        assert_eq!(compress(&[1, 2, 3, 4]), vec![0x63, 1, 0xFF]);

        assert_eq!(
            compress(&[1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]),
            vec![0x63, 1, 0xC7, 0x4, 0xFF]
        );

        // a relative inverted backreference can only be encoded as an extended command
        assert_eq!(
            compress(&[1, 2, 3, 4, !1, !2, !3, !4, 1, 2, 3, 4]),
            vec![0x63, 1, 0xFC, 0x07, 0x4, 0xFF]
        );

        // a relative inverted backreference must not collide with the stop symbol
        let tricky = [1, 2, 3, 4]
            .into_iter()
            .chain([!1, !2, !3, !4, 1, 2, 3, 4].into_iter().cycle().take(0x400))
            .collect::<Vec<u8>>();
        assert_eq!(compress(&tricky), [0x63, 1, 0xF7, 0xFF, 0, 0, 0xFF]);

        // create a 512-byte-long non-compressible sequence
        let seq = || {
            std::iter::successors(Some(1u8), |&x| Some(x.wrapping_add(3)))
                .take(256)
                .flat_map(|i| [i, i.wrapping_sub(1)])
        };
        assert_eq!(
            compress(&seq().chain(seq()).collect::<Vec<u8>>()),
            [0xE1, 0xFF]
                .into_iter()
                .chain(seq())
                .chain([0xF1, 0xFF, 0x00, 0x00, 0xFF])
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_roundtrip_green_brinstar() {
        let data = include_bytes!("green_brinstar_main_shaft.bin");

        let decompressed = decompress(data).unwrap();
        let recompressed = compress(&decompressed);
        let redecompressed = decompress(&recompressed).unwrap();

        assert_eq!(decompressed, redecompressed);
    }
}
