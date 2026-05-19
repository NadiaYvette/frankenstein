// Demonstrates Rust's field-spec placeholders through the Frankenstein
// bridge: width, alignment, zero-pad, fill char.
//
// Rust encodes specs in the template byte buffer:
//   c0           plain placeholder
//   c1 <spec>{4} placeholder + 4-byte spec (no width)
//   c3 <spec>{4} <width>{2}   placeholder + spec + LE u16 width
//
// Spec bytes:
//   [0] fill char           (default 0x20 = ' ')
//   [1] reserved/0
//   [2] flags: bit 5 = '+' sign, bit 7 = '#' alternate
//   [3] align byte:
//         high nibble: 0=left, 2=right, 4=center, 6=default
//         bit 3: has-width (also signalled by c3 vs c1)
//         bit 0: zero-pad flag
//
// See kk_rust_print_arg_with_spec in runtime/kk_runtime.c for the
// applied-formatting logic (sign-aware zero-pad, default alignment
// per-type, custom fill chars).

#[no_mangle]
pub fn id(n: i64) -> i64 { n }

pub fn main() {
    let n = id(42);
    let neg = id(-7);
    let s = "hi";

    println!("[{:5}]", n);       //  [   42]   width-only, default right-align
    println!("[{:<5}]", n);      //  [42   ]   left-align
    println!("[{:>5}]", n);      //  [   42]   right-align (explicit)
    println!("[{:^5}]", n);      //  [ 42  ]   center
    println!("[{:05}]", n);      //  [00042]   zero-pad
    println!("[{:05}]", neg);    //  [-0007]   sign-aware zero-pad
    println!("[{:x>5}]", n);     //  [xxx42]   custom fill 'x'
    println!("[{:08x}]", id(255)); // [000000ff]  hex zero-padded
    println!("[{:5}]", s);       //  [hi   ]   strings default left-align
    let long = "frankenstein";
    println!("[{:.5}]", long);   //  [frank]   precision truncates strings
    println!("[{:.5}]", n);      //  [00042]   precision = min digits (ints)
    println!("[{:10.5}]", long); //  [frank     ]  width + precision
    println!("[{:+}]", n);       //  [+42]     explicit + for non-negative
    println!("[{:+}]", neg);     //  [-7]      negative unchanged
    println!("[{:+05}]", n);     //  [+0042]   sign + zero-pad
}
