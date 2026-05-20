// Demonstrates `println!` for f64 / f32 floats.
//
// Float support:
//   - `{}` Display: prints the shortest round-trip representation
//     (`%g`-style for fractions, whole numbers as integers)
//   - `{:.N}` precision: N decimal places (`%.Nf`)
//   - `{:+}` sign flag: prepends `+` for non-negative values
//   - `{:W.Pf}` width + precision: pad to width W with P decimals
//
// f64 literals carry their IEEE bits in the i64 LitInt; the bridge
// detects `<num>f64` / `<num>f32` suffixes in parseConstLit and
// performs the bit-cast.  rust_arg_f64 / rust_arg_f32 tag the
// runtime cell so the printer reinterprets bits-as-float.

pub fn main() {
    let pi: f64 = 3.14159;
    let zero: f64 = 0.0;
    let neg: f64 = -2.5;
    let small: f32 = 0.5;
    let half: f64 = 3.5;
    println!("{}", pi);
    println!("{}", zero);
    println!("{}", neg);
    println!("{}", small);
    println!("{:.2}", pi);
    println!("{:.0}", half);
    println!("{:+}", pi);
    println!("{:10.2}", pi);
}
