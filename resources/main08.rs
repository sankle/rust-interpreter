use std::io;
use std::io::Write;

fn factorial(n: i64) -> i64 {
    if n < 2 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main() {
    println!("****************************************************");
    println!("Se ingresa un valor entero, se muestra su factorial.");
    println!("Se utiliza un algoritmo recursivo.");
    println!("****************************************************");

    print!("n: ");
    io::stdout().flush().expect("Error de escritura!");

    let mut renglon: String = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let n: i64 = renglon
        .trim()
        .parse::<i64>()
        .expect("Se esperaba un numero entero!");

    if n < 0 {
        println!("ERROR: El algoritmo requiere un numero entero no negativo!");
    } else {
        println!("{}! es {}", n, factorial(n));
    }
}