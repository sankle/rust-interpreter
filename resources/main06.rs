use std::io;
use std::io::Write;

fn raiz_cuadrada_de_positivo(x: f64) -> f64 {
    if x == 1.0 {
        return 1.0;
    }

    let mut izq: f64;
    let mut der: f64;

    if x < 1.0 {
        izq = x;
        der = 1.0;
    } else {
        izq = 1.0;
        der = x;
    }

    let mut r: f64 = (izq + der) / 2.0;

    while f64::abs(x - r * r) > 0.0000000001 {
        if r * r < x {
            izq = r;
        } else {
            der = r;
        }

        r = (izq + der) / 2.0;
    }

    r
}

fn main() {
    println!("**********************************************************");
    println!("Se ingresa un valor numerico, se muestra su raiz cuadrada.");
    println!("Se utiliza el algoritmo de la biseccion.");
    println!("**********************************************************");

    print!("x: ");
    io::stdout().flush().expect("Error de escritura!");

    let mut renglon: String = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let x: f64 = renglon.trim().parse::<f64>().expect("Se esperaba un numero!");

    if x == 0.0 {
        println!("La raiz cuadrada de 0 es 0.00000000");
    } else {
        let rc: f64 = raiz_cuadrada_de_positivo(f64::abs(x));

        if x < 0.0 {
            if x == -1.0 {
                println!("Las raices cuadradas de -1 son +i y -i");
            } else {
                println!(
                    "Las raices cuadradas de {} son +{:.8}i y -{:.8}i",
                    x, rc, rc
                );
            }
        } else {
            println!("Las raices cuadradas de {} son +{:.8} y -{:.8}", x, rc, rc);
        }
    }
}
