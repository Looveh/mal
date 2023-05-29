use std::io::{self, BufRead, Write};

fn read(s: String) -> String {
    s
}

fn eval(s: String) -> String {
    s
}

fn print(s: String) -> String {
    s
}

fn rep(s: String) -> String {
    print(eval(read(s)))
}

fn main() {
    loop {
        print!("user> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();

        io::stdin().lock().read_line(&mut line).unwrap();

        let res = rep(line);

        print!("{res}");
    }
}
