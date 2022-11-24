use std::io;
use std::io::Write;
use std::process;

fn dividir(x: i64, y: i64, q: &mut i64, r: &mut i64) {
    if y == 0 {
        println!("ERROR: Division por cero!");
        process::exit(1);
    }

    *q = 0;
    *r = x;
    if *r < 0 {
        *r = -*r;
    }

    let v: i64;
    let mut w: i64;
    if y >= 0 {
        v = y;
        w = y;
    } else {
        v = -y;
        w = -y;
    }

    while w <= *r {
        w *= 2;
    }

    while w > v {
        *q *= 2;
        w /= 2;
        if w <= *r {
            *r -= w;
            *q += 1;
        }
    }

    if x < 0 {
        *r = -*r;
        *q = -*q;
    }

    if y < 0 {
        *q = -*q;
    }
}

fn mostrar_salida(cociente: i64, resto: i64) {
    println!("Cociente: {}", cociente);
    println!("Resto: {}", resto);
}

fn main() {
    println!("**************************************************************");
    println!("Se ingresan dos valores enteros, se muestra su cociente.");
    println!("Se utiliza el algoritmo 'desplazar y restar' (shift-subtract).");
    println!("**************************************************************");

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

    print!("y: ");
    io::stdout().flush().expect("Error de escritura!");

    renglon = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let y: i64 = renglon
        .trim()
        .parse::<i64>()
        .expect("Se esperaba un numero entero!");

    let mut q: i64 = 0;
    let mut r: i64 = 0;

    dividir(x, y, &mut q, &mut r);

    mostrar_salida(q, r);
}

