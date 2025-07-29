mod constants;
mod generator;
mod io_handler;
mod lexer;
mod parser;
mod target;
mod token;

fn main() {
    if let Err(e) = run() {
        println!("{}", e);
        std::process::exit(1)
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cmd_args = match io_handler::process_cmd_args()? {
        Some(args) => args,
        None => return Ok(()),
    };
    let contents = io_handler::read_file(&cmd_args.file_name)?;

    let tokens = lexer::parse_text(&contents)?;
    let program = parser::parse_program(tokens)?;
    let out_assembly = generator::generate(&cmd_args.target, program)?;
    let out_file = &cmd_args.get_file_name()?;
    io_handler::write_program(&out_file, &out_assembly)?;
    Ok(())
}
