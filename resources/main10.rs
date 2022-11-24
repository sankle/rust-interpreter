use std::io;
use std::io::Write;

fn entero_a_hexa(n: i64) -> String {
    let mut hexa: String = String::from("0");

    if n != 0 {
        hexa = String::new();
    }

    let digitos: String = String::from("0123456789ABCDEF");

    let mut cociente: i64 = n;

    while cociente != 0 {
        let resto: i64 = cociente % 16;

        let ch: char = digitos.as_str().chars().nth(resto as usize).unwrap();

        hexa = format!("{}{}", ch, hexa);

        cociente /= 16;
    }

    hexa
}

fn main() {
    println!("*********************************************************************************************");
    println!("Se muestran 16 numeros en decimal y en hexadecimal, al presionar Enter se muestran 16 mas.");
    println!("Se sale del programa escribiendo 'salir' y presionando Enter.");
    println!("*********************************************************************************************");

    let mut opcion: String = String::new();

    let mut num: i64 = 0;

    while opcion != "salir" && opcion != "SALIR" {
        println!("\nDEC\tHEX");
        println!("---\t---");

        let mut cont: i64 = 0;

        while cont < 16 {
            println!("{}\t{}", num, entero_a_hexa(num));
            num += 1;
            cont += 1;
        }

        print!("Presione Enter o escriba 'salir' y presione Enter: ");
        io::stdout().flush().expect("Error de escritura!");

        opcion = String::new();
        io::stdin()
            .read_line(&mut opcion)
            .expect("Error de lectura!");
        opcion = opcion.trim().to_string();
    }
}
