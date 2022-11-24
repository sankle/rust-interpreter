use std::io;
use std::io::Write;

const TRES: i64 = 3;

fn es_impar_primo(n: i64) -> bool {
    let mut es_p: bool = true;
    let limite: i64 = f64::sqrt(n as f64) as i64;
    let mut d: i64 = TRES;
    while d <= limite && es_p {
        if n % d == 0 {
            es_p = false;
        }
        d += 2;
    }

    es_p
}

fn main() {
    println!("*********************************************************************************************");
    println!("Se ingresa un valor entero positivo, se muestran los numeros primos menores que ese valor.");
    println!("Se utiliza una funcion booleana para determinar si un numero impar mayor que 1 es primo o no.");
    println!("*********************************************************************************************");

    print!("x: ");
    io::stdout().flush().expect("Error de escritura!");

    let mut renglon: String = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let x: i64 = renglon
        .trim()
        .parse::<i64>()
        .expect("Se esperaba un numero entero!");

    if x <= 2 {
        print!("No hay numeros primos menores que {}", x);
    } else {
        print!("Numeros primos menores que {}: 2", x);

        let mut n: i64 = TRES;

        while n < x {
            if es_impar_primo(n) {
                print!(" {}", n);
            }
            n += 2;
        }
    }

    println!();
}
