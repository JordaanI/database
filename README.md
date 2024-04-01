# BroodB Database System

The BroodB database system is a unique implementation designed for efficient and scalable storage. It utilizes a chord system for data distribution and management, optimizing resource usage and ensuring data integrity.

## Features

- **Chord System:** Positions in the database are assigned based on the load on the node.
- **Load Calculation:** Load on a node is determined by the amount of available space and the amount of space taken by the data stored on that node.
- **On-demand Data Retrieval:** Only the node that contains the requested record is read into memory.
- **Centralized History:** A complete history of additions is kept in a central file.
- **Versioning:** Updates are handled by versions of the database. Rewrites node entries to only contain the most up-to-date record.

## Getting Started

To use the BroodB database system, follow these steps:

1. Clone the repository.
2. Install the required dependencies.
3. Run the setup script to configure the system.
4. Start the database server.

## Usage

- For basic operations, refer to the documentation.
- Advanced configurations can be done by modifying the configuration files.

## Contributing

We welcome contributions from the community.

## License

None yet, but I wont sue. I promise.
