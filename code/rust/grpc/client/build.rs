fn main() -> Result<(), Box<dyn std::error::Error>> {
    tonic_build::configure()
        .build_server(false)
        .build_client(true)
        .out_dir("src")
        .compile(
            &["../../../shared/protos/tasks.proto"],
            &["../../../shared/protos"],
        )?;
    Ok(())
}