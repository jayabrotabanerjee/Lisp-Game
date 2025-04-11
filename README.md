# Cosmic Lisp Explorer

Cosmic Lisp Explorer is a 2D roguelike space exploration game where you navigate through procedurally generated star systems, encounter alien species, collect resources, and upgrade your ship while surviving in the vast cosmos.

## Game Overview

In Cosmic Lisp Explorer, you command a small spacecraft exploring the uncharted regions of space. Each playthrough generates a unique galaxy with different star systems, planets, and challenges. The game features turn-based movement and combat with permadeath mechanics, making each decision critical to your survival.

![Game Screenshot Placeholder]

**Key Features:**
- Procedurally generated galaxies with unique star systems
- Resource management and ship upgrades
- Turn-based exploration and combat
- Dynamic alien factions with different behaviors
- ASCII-based graphics with optional enhanced visual mode

## Technology Stack

This game is primarily built with Common Lisp, with some additional technologies:

- **Core Game Logic**: Common Lisp (SBCL 2.3.0+)[1]
- **Graphics Engine**: cl-raylib for enhanced visuals[1]
- **Terminal UI**: cl-charms (ncurses wrapper)[2]
- **Sound Effects**: SDL2 audio bindings[1]
- **Build System**: ASDF with Quicklisp[3]
- **Optional JavaScript UI**: Parenscript for web export[1][3]

## Installation

### Prerequisites

- SBCL (Steel Bank Common Lisp) 2.3.0+
- Quicklisp package manager
- libraylib-dev (for enhanced graphics mode)
- libncurses-dev (for terminal interface)
- SDL2 development libraries

### Step-by-Step Installation

1. **Install SBCL and required libraries:**

```bash
# For Debian/Ubuntu
sudo apt install sbcl libraylib-dev libncurses-dev libsdl2-dev

# For macOS (using Homebrew)
brew install sbcl raylib ncurses sdl2

# For Windows
# Download SBCL installer from http://www.sbcl.org/platform-table.html
# Install raylib, ncurses, and SDL2 via MSYS2
```

2. **Install Quicklisp (if not already installed):**

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
```

3. **Clone the repository:**

```bash
git clone https://github.com/yourusername/cosmic-lisp-explorer.git
cd cosmic-lisp-explorer
```

## Running the Game

### Terminal Method

The simplest way to run Cosmic Lisp Explorer is directly from your terminal:

```bash
sbcl --load run.lisp
```

This will start the game using the terminal interface with ASCII graphics.

### Enhanced Graphics Mode

To run with enhanced graphics using raylib:

```bash
sbcl --load run-enhanced.lisp
```

### Creating a Standalone Executable

You can build a standalone executable with:

```bash
sbcl --load build.lisp
```

This creates an executable in the `./bin` directory that you can run directly:

```bash
./bin/cosmic-lisp-explorer
```

## Game Controls

### Terminal Mode
- **Arrow Keys/WASD**: Move your ship
- **Space**: Interact/Confirm
- **I**: Open inventory
- **M**: Open map
- **E**: Use equipment
- **Q**: Quit game
- **S**: Save game (only at space stations)

### Enhanced Graphics Mode
Same controls as Terminal Mode with additional:
- **Mouse**: UI interaction
- **Scroll Wheel**: Zoom in/out
- **R**: Toggle radar view

## Project Structure

```
cosmic-lisp-explorer/
├── src/                     # Source code
│   ├── main.lisp            # Entry point
│   ├── game/                # Game mechanics
│   ├── entities/            # Game entities
│   ├── ui/                  # User interface
│   ├── graphics/            # Graphics rendering
│   ├── audio/               # Sound system
│   └── utils/               # Utility functions
├── assets/                  # Game assets
│   ├── sprites/             # Visual assets
│   ├── audio/               # Sound files
│   └── data/                # Game data files
├── lib/                     # Third-party libraries
├── tests/                   # Test suite
├── build.lisp               # Build script
├── run.lisp                 # Run script (terminal)
├── run-enhanced.lisp        # Run script (raylib)
└── README.md                # This file
```

## Development

This project was created for the Spring Lisp Game Jam 2025. It uses a combination of different libraries and frameworks from the Common Lisp ecosystem[1][2].

### Key Components

- **Game Engine**: Custom-built using functional programming principles
- **Procedural Generation**: Based on perlin noise and cellular automata
- **UI System**: Terminal-based with cl-charms, with optional raylib graphics
- **Entity Component System**: For game object management
- **Save System**: Uses S-expressions for game state serialization

### Adding New Content

The game is designed to be easily extensible. New alien species, ship components, or celestial objects can be added by creating new definition files in the `assets/data/` directory using S-expression syntax.

Example of defining a new alien species:

```lisp
(define-species :name "Xenomorphs"
                :hostility 0.8
                :trade-affinity 0.2
                :tech-level 7
                :description "Highly aggressive silicon-based lifeforms that thrive in vacuum.")
```

## Troubleshooting

### Common Issues

1. **Missing Libraries**: If you encounter "Unable to load foreign library" errors, ensure all dependencies are installed.

2. **Display Issues**: For terminal mode, ensure your terminal supports Unicode and has a compatible font.

3. **Performance Problems**: On slower systems, you can reduce graphical settings in the options menu or use the `--low-graphics` flag when launching.

## License

This project is licensed under the GNU GPL v3.0 License - see the LICENSE file for details.

## Acknowledgments

- Inspired by games showcased in previous Lisp Game Jams[1]
- Built with libraries from the Common Lisp games ecosystem[2]
- Special thanks to the Common Lisp community for their support and feedback

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request
