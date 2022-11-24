use std::io;
use std::io::Write;

fn main() {
    println!("************************************************************");
    println!("Se ingresan dos valores enteros, se muestra su producto.");
    println!("Se utiliza el algoritmo de 'multiplicacion por duplicacion'.");
    println!("(Metodo campesino ruso de multiplicacion)");
    println!("************************************************************");

    print!("x: ");
    io::stdout().flush().expect("Error de escritura!");

    let mut renglon: String = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let mut x: i64 = renglon
        .trim()
        .parse::<i64>()
        .expect("Se esperaba un numero entero!");

    let mut x_cambio: bool = false;
    if x < 0 {
        x = -x;
        x_cambio = true;
    }

    print!("y: ");
    io::stdout().flush().expect("Error de escritura!");

    renglon = String::new();
    io::stdin()
        .read_line(&mut renglon)
        .expect("Error de lectura!");
    let mut y: i64 = renglon
        .trim()
        .parse::<i64>()
        .expect("Se esperaba un numero entero!");

    let mut y_cambio: bool = false;
    if y < 0 {
        y = -y;
        y_cambio = true;
    }

    let mut prod: i64 = 0;

    while y > 0 {
        if y % 2 != 0 {
            prod += x;
        }
        x *= 2;
        y /= 2;
    }

    if x_cambio {
        prod = -prod;
    }

    if y_cambio {
        prod = -prod;
    }

    println!("x*y={}", prod);
}
