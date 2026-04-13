## Process Diagram (Data Flow, Agentic Orchestration, RAG, Fallback)

```mermaid
flowchart TD
  U[User] -->|Choose section + inputs| UI[Shiny UI]
  UI -->|Fetch button| FE[Fetch event handler]
  FE --> CALL[Alpha Vantage API call]
  CALL -->|Success| PARSE[Parse + normalize response]
  CALL -->|Rate limit / API error| FB{Cached result exists?}
  FB -->|Yes| CACHEUSE[Use cached data + show fallback banner]
  FB -->|No| ERR[Show API error message]
  PARSE --> STORE[Store in reactive values + update cache]
  CACHEUSE --> STORE
  STORE --> VIZ[Interactive visuals + calculators]

  UI -->|Generate report| AI[AI Reporter]
  AI --> CTX[Build application context from reactive data]
  AI --> RQ[Build RAG query]
  RQ --> CORPUS[(data/rag_market_corpus.txt)]
  CORPUS --> RET[Keyword retrieval top-k chunks]
  RET --> RCTX[RAG text + retrieval trace]
  CTX --> COMB[Combined context]
  RCTX --> COMB

  subgraph ORCH["Multi-agent pipeline (3 agents)"]
    COMB --> A1[Agent 1: Orchestrator<br/>themes, gaps, delegation]
    A1 --> A2[Agent 2: Market Analyst<br/>facts, interpretation, risks]
    A2 --> A3[Agent 3: Lead Editor<br/>final concise brief]
  end

  A3 --> OUT[Final AI brief in UI]
  A1 --> TRACE[Agent trace panels]
  A2 --> TRACE
  RCTX --> TRACE

  AI --> LLMFB{Primary LLM provider works?}
  LLMFB -->|Yes| ORCH
  LLMFB -->|No| LLMALT[Fallback to alternate provider]
  LLMALT --> ORCH
```

### Required-feature mapping

- **Agentic orchestration**: 3 role-specific agents with sequential coordination.
- **RAG**: retrieval from custom local data source `data/rag_market_corpus.txt`.
- **Functional app resilience**: Alpha Vantage rate-limit fallback to cached results.
- **UI/visual design**: interactive plots, tooltips, legends, and what-if calculators.

