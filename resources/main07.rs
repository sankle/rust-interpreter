fn mostrar_espacios(n: i64) {
    let mut i: i64 = 0;

    while i < n {
        print!(" ");
        i += 1;
    }
}

fn main() {
    println!("*******************************************************");
    println!("Se muestra una sinusoide graficada mediante asteriscos.");
    println!("El programa no utiliza 'use'.");
    println!("*******************************************************");

    println!("x\t sin(x)");

    let mut a: f64 = 0.0;

    let lim: f64 = 8.0 * f64::atan(1.0);

    while a < lim {
        let s: f64 = f64::sin(a);
        if s >= 0.0 {
            print!("{:.2}\t {:.5}", a, s);
        } else {
            print!("{:.2}\t{:.5}", a, s);
        }
        mostrar_espacios((25.0 + 24.0 * s) as i64);
        println!("*");
        a += 0.1;
    }

    print!("{:.2}\t {:.5}", lim, 0.0);
    mostrar_espacios(25);
    println!("*");
}
