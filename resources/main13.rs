fn duplicar(x: &mut i64) {
    *x <<= 1;
}

fn main() {
    let mut w: i64;
    w = 7;
    w <<= 1;
    println!("1- w vale: {}", w);
    duplicar(&mut w);
    println!("2- w vale: {}", w);
}
