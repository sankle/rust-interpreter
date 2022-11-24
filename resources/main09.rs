use std::io;
use std::io::Write;

fn potencia_rec(x: f64, n: i64) -> f64 {
    if n == 0 {
        return 1.0;
    }
    if n % 2 == 0 {
        let m: f64 = potencia_rec(x, n / 2);
        m * m
    } else {
        x * potencia_rec(x, n - 1)
    }
}

fn potencia(x: f64, n: i64) -> f64 {
    let p: f64;
    if n < 0 {
        p = potencia_rec(x, -n);
        1.0 / p
    } else {
        potencia_rec(x, n)
    }
}

fn main() {
    println!("*******************************************************************************************");
    println!("Se ingresan el valor de una base y el valor entero de un exponente, se muestra la potencia.");
    println!("Se utiliza un algoritmo recursivo.");
    println!("*******************************************************************************************");

    print!("b: ");
    io::stdout().flush().expect("Error de escritura!");

    let mut renglon: String = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let b: f64 = renglon.trim().parse::<f64>().expect("Se esperaba un numero!");

    print!("e: ");
    io::stdout().flush().expect("Error de escritura!");

    renglon = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let e: i64 = renglon
        .trim()
        .parse::<i64>()
        .expect("Se esperaba un numero entero!");

    println!("{} elevado a la {} es {}", b, e, potencia(b, e));
}
