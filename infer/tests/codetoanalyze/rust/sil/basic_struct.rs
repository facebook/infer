fn main() {
    struct Point {
        x: i32,
        y: i32,
    }
    
    let mut p = Point { x: 1, y: 2 };
    let one = p.x;
    let two = p.y;
    p.x = two;
    p.y = one;
}