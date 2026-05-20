// Demonstrates `println!("{:?}", v)` for `#[derive(Debug)]` enums
// across all three variant shapes:
//
//   - Unit variant:        Shape::Origin             → "Origin"
//   - Tuple variant:       Shape::Circle(10)         → "Circle(10)"
//   - Struct variant:      Shape::Rect { w, h }      → "Rect { w: 7, h: 13 }"
//
// All three are recognised by MirParse's parseEnumTupleCtor /
// parseEnumUnitCtor / parseStructCtor and emit RvStruct rvalues
// carrying the variant's last-path segment (just `Circle`, not
// `Shape::Circle`) as the metadata name.  The runtime Debug
// printer reads the type-name + field-names metadata from a
// KK_RUST_STRUCT_TAG cell and renders Rust-faithful output.

#[derive(Debug)]
pub enum Shape {
    Circle(i64),
    Rect { w: i64, h: i64 },
    Origin,
}

pub fn main() {
    let a = Shape::Circle(10);
    let b = Shape::Rect { w: 7, h: 13 };
    let c = Shape::Origin;
    println!("{:?}", a);
    println!("{:?}", b);
    println!("{:?}", c);
}
