# Loop Linter TODOs

## Plugin Improvements

- [ ] **Enhance `findProducer` cycle handling**: 
    In `LoopLinter/Plugin.hs`, the `findProducer` function currently returns `Nothing` when it detects a cycle in variable aliases (e.g., `x = y; y = x`). 
    We should add a warning (e.g., `liftIO $ putStrLn "Warning: Alias cycle detected..."`) before returning `Nothing`. This helps identify tight wiring loops that have no functional producer.

## Notes

- **All recursive bindings in a expr are in let rec blocks**:
    We can use this fact to create graphs for each top level binding, and then traverse the graph to find tight loops.
- We will have to save the information about each top level binding, whether it is registered or not.
- This can be used in later phases to detect loops.
- The graph is essentially a map from id to vars(more ids - the neighbours) on rhs, 
  and id to the producer funtion(the edge "weight"). 

## TODOs

- [x] Now that we are able to extract ids and func names, extract the vars that are used in the expr (in rhs)
- [x] once we have vars, we can proceed to build a graph 
- [x] then traverse it to find tight loops (traversal and detection function)
- [x] **Cross-module support for `hasReg`**: Persist `hasReg` information for each module to disk and load it when analyzing importing modules. 
        This ensures combinational loop detection works correctly across module boundaries.
- [ ] **Use GHC Warnings**: Replace `liftIO $ putStrLn` with proper GHC diagnostics/warnings so they are integrated into the compiler's output and can be treated as errors if `-Werror` is set.
- [ ] **Bug**: currently if mealy moore or register is used inside a function we are running the detection on, it does not trigger the break edge condition,
              because it will not be found in the hasReg registry (instead we should check against the primitives list).
- [ ] optimize the graph building algorithm and detection algorithms
