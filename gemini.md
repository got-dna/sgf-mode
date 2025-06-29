The `sgf-mode` library is an Emacs Lisp collection designed for interacting with Smart Game Format (SGF) files, primarily used for recording Go games. It provides functionalities for parsing, displaying, navigating, editing, and analyzing SGF files within Emacs.

### Key Features:

*   **SGF Parsing and Serialization:** Reads SGF content into Emacs Lisp data structures and writes game states back to SGF format.
*   **Interactive Game Navigation:** Commands to move forward/backward through moves, jump to specific moves, and traverse variations.
*   **Graphical Board Display (SVG):** Renders the Go board and stones as scalable vector graphics (SVG) within Emacs buffers, including move numbers, marks, and hints.
*   **Game Tree Visualization:** Generates ASCII tree graphs of game variations, allowing easy navigation and understanding of complex game trees.
*   **Game Editing:** Functions to edit move comments, annotations, game information, and setup stones.
*   **KataGo Integration:** Connects with the KataGo AI Go engine for game analysis, displaying win rates, score leads, and principal variations.
*   **Org-mode Babel Support:** Enables embedding and executing SGF content directly within Org-mode files.

### Modules Overview:

*   `sgf-mode.el`: The core major mode for SGF files. It orchestrates the functionalities from other modules, providing interactive commands for game navigation, editing, and integration with KataGo. It manages the game state, overlays, and updates the graphical display.
*   `sgf-io.el`: Handles the input and output of SGF data. It parses SGF content from strings or files into a syntax tree and then into a doubly-linked list of nodes representing the game tree. It also serializes the game state back into SGF format and can generate JSON for KataGo analysis.
*   `sgf-svg.el`: Responsible for the visual representation of the Go board. It uses Emacs's SVG capabilities to draw the board, stones, move numbers, marks, hints, and KataGo analysis overlays.
*   `sgf-util.el`: Contains utility functions and data structures used across the library. This includes functions for board manipulation (e.g., `sgf-board-get`, `sgf-board-set`), game logic (e.g., `sgf-valid-move-p`, `sgf-capture-stones`), and definitions for game state and linked node objects. It also defines customizable variables for display options and game behavior.
*   `sgf-graph.el`: Focuses on visualizing the SGF game tree as an ASCII graph. It generates a separate buffer displaying the tree structure, allowing users to see all variations and navigate through them.
*   `katago.el`: Provides the interface for communicating with the KataGo AI engine. It handles starting KataGo processes, sending analysis queries, and parsing KataGo's JSON output to integrate analysis results into the SGF game state.
*   `ob-sgf.el`: Implements Org-mode Babel support for SGF code blocks. This allows users to embed SGF content directly into Org-mode files and have it rendered and interacted with using `sgf-mode`'s functionalities.


### tests
*   tests are in the tests directory.
