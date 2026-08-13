### Welcome to my strategy musings 👋

The repository contains my musings on systematic strategies, financial markets (CeFi/DeFi) and AI.

- 🔭 I'm currently focused on systematic investment strategies and computer science, particularly AI to solve real-world problems.
- 📈 I've implemented a lot of strategies and will start sharing them here for public use.
- 💬 Ask me about: Financial markets and strategies, FinTech, AI and Regenerative Agriculture 🌱🌴🌿😃
- 📫 How to reach me: rashid.rasul@me.com
- ⚡ Fun fact: I like to hike and kayak 🚣🏽‍♀

Highly recommend checking out the forked Courses repository 🎓

P.S. I ❤️ **Rust**

---

## 🦀 Rust crate experience

My Rust work runs from small CLI/data utilities in these public repos through to a private, local-first quantitative trading and agent system built on three Cargo workspaces (WASM-sandboxed strategy execution, async orchestration, and vector-backed memory). The major crates I reach for, and what I use them for:

| Domain | Crates | How I use them |
|--------|--------|----------------|
| **Async & web** | `tokio`, `axum`, `actix-web`, `tokio-stream` | Async runtimes and HTTP service layers for agents, APIs and Lambda handlers. |
| **WASM & sandboxing** | `wasmtime` | Runs untrusted trading-strategy "skills" in a memory/time/syscall-bounded WASM sandbox (WASI Preview 1). |
| **Persistence & search** | `sqlx` (+ pgvector), `sqlite` | Postgres access and vector similarity search (HNSW) over embeddings; SQLite for local pipelines. |
| **Serialisation & data** | `serde`, `serde_json`, `csv`, `apache-arrow`, `polars` | Config/JSON/CSV I/O and fast columnar DataFrame processing. |
| **CLI & observability** | `clap`, `anyhow`, `tracing`, `tracing-subscriber`, `log`, `env_logger` | Argument parsing, ergonomic error handling, and structured logging/telemetry. |
| **Cloud / DevOps** | `lambda_runtime`, `lambda_http`, `aws-sdk-s3`, `aws-sdk-dynamodb`, `aws-config`, `glob` | Serverless Rust on AWS and CI/CD automation. |
| **ML & GPU** | `tch`, `rust-bert`, `tract-onnx`, `ndarray`, `image`, `cuda-runtime-sys`, `cublas-sys`, `safetensors` | PyTorch bindings, transformer/ONNX inference, and low-level CUDA/cuBLAS FFI. |
| **Concurrency & algorithms** | `rayon`, `petgraph` | Data-parallelism and graph algorithms (PageRank, centrality, shortest-path). |
| **Interop & utilities** | `pyo3`, `chrono`, `rand`, `regex`, `flate2`, `sha3`, `criterion` | Rust↔Python bindings, time/RNG/regex, compression, hashing and benchmarking. |

Design principles I hold to in production Rust: **default-deny egress, zero `unwrap()` on production paths, WASM isolation for untrusted code, and version-pinned reproducible builds.**

Repos worth a look: [`rust-cli-utils`](https://github.com/rstrategist/rust-cli-utils) · [`rust-data-engineering`](https://github.com/rstrategist/rust-data-engineering) · [`rust-mlops`](https://github.com/rstrategist/rust-mlops) · [`rust-pytorch-gpu-template`](https://github.com/rstrategist/rust-pytorch-gpu-template) · [`rust-systems-programming`](https://github.com/rstrategist/rust-systems-programming)
