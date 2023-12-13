# Minswap-Dex Matchmaker

This repository facilitates the creation of transactions for token swapping within the Minswap-Dex ecosystem.

## Getting Started

### Setting Up Blockfrost

1. Create a project using [Blockfrost](https://blockfrost.io/) to generate a `project_id`.

2. Navigate to the `/minswap-dex` directory.

3. Create a `secrets` folder:
    ```bash 
    mkdir secrets
    ```

4. Move into the `secrets` directory and create a file containing your `project_id`:
    ```bash
    cd secrets 
    ```
    ```bash 
    echo "project_id_here" > blockfrost.mainnet.token
    ```

5. Return to the `/minswap-dex` directory.

6. Export your Blockfrost Token path. Example:
    ```bash
    export BLOCKFROST_TOKEN_PATH=/home/user/minswap-dex/secrets/blockfrost.mainnet.token
    ```

### Running the Matchmaker

1. Run [Kuber-Server](https://github.com/dQuadrant/kuber) locally from the `old-kuber` branch.

2. In the `Main.hs` file of `Minswap-Dex`, modify the `remoteKuberConnection` function to use the appropriate endpoint where the `kuber-server` is running.

3. Run the matchmaker:
    ```bash
    cabal run matchmaker-prototype/
    ```

4. Upon finding a matching order, the transaction details, including the transaction hash, will be displayed in the console.

### More Information
- For more information on the details and operation of the Minswa-Dex, refer to [Minswap-Contracts](https://github.com/CatspersCoffee/contracts)
