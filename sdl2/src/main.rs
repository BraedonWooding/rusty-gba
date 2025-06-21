use core::time;
use sdl2::{
    Sdl,
    event::{self, Event},
    keyboard::Scancode,
};
use std::{fs::File, ops::Div, sync::Arc};
use tracing::info;
use tracing_subscriber::{filter, prelude::*};

struct Config {
    pub vsync: bool,
    // Can be set from any value > 0 to enable FPS limiting.
    // Use vsync == false to disable fps limiting.
    pub fps: u32,
}

fn main() {
    setup_tracing();

    let config = Config {
        vsync: true,
        fps: 60,
    };

    let context = start_sdl2();

    main_loop(context, &config).expect("Failed to run main loop");
}

fn main_loop(context: Sdl, config: &Config) -> Result<(), String> {
    let mut event_pump = context.event_pump()?;
    info!("Entering main loop...");

    loop {
        let start_time = std::time::Instant::now();
        for event in event_pump.poll_iter() {
            info!("Received event: {:?}", event);
            match event {
                Event::Quit { .. } => {
                    return Ok(());
                }
                Event::KeyUp {
                    scancode: Some(scancode),
                    ..
                } => {
                    info!("Key released: {:?}", scancode);
                    if scancode == Scancode::Space {
                        return Ok(());
                    }
                }
                Event::DropFile { filename, .. } => {
                    todo!("impl DropFile again {}", filename);
                }
                _ => {}
            }
        }

        if config.vsync {
            let time_passed = start_time.elapsed();
            let delay = time::Duration::new(1, 0).div(config.fps as u32);
            match delay.checked_sub(time_passed) {
                Some(remaining) => {
                    spin_sleep::sleep(remaining);
                }
                None => {}
            }
        }
    }
}

fn setup_tracing() {
    let stdout_log = tracing_subscriber::fmt::layer().pretty();

    // A layer that logs events to a file.
    let file = File::create("debug.log");
    let file = match file {
        Ok(file) => file,
        Err(error) => panic!("Error: {:?}", error),
    };
    let debug_log = tracing_subscriber::fmt::layer().with_writer(Arc::new(file));
    tracing_subscriber::registry()
        .with(
            stdout_log
                // Add an `INFO` filter to the stdout logging layer
                .with_filter(filter::LevelFilter::INFO)
                // Combine the filtered `stdout_log` layer with the
                // `debug_log` layer, producing a new `Layered` layer.
                .and_then(debug_log),
        )
        .init();
}

fn start_sdl2() -> Sdl {
    let span = tracing::info_span!("SDL2 Initialization");
    let _enter = span.enter();

    info!("Starting SDL2...");

    let context = sdl2::init().expect("Failed to initialize SDL2");

    info!("Initialized SDL2 successfully!");

    context
}
