import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import generic.stl.Pair;
import ghidra.app.script.GhidraScript;
import ghidra.graph.GDirectedGraph;
import ghidra.graph.GraphFactory;
import ghidra.program.model.address.Address;
import ghidra.program.model.address.AddressRange;
import ghidra.program.model.address.AddressSetView;
import ghidra.program.model.block.BasicBlockModel;
import ghidra.program.model.block.CodeBlock;
import ghidra.program.model.block.CodeBlockIterator;
import ghidra.program.model.block.CodeBlockReference;
import ghidra.program.model.block.CodeBlockReferenceIterator;
import ghidra.program.model.block.graph.CodeBlockEdge;
import ghidra.program.model.block.graph.CodeBlockVertex;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.listing.Variable;
import ghidra.program.model.symbol.Symbol;

public class GenGround extends GhidraScript {

    @Override
    protected void run() throws Exception {
        // get first argument as output filepath
        String inputPath = getScriptArgs()[0];
        String outputPath = getScriptArgs()[1];

        Set<String> funcs = new HashSet<String>();
        // open inputPath and read line
        File input = new File(inputPath);
        java.util.Scanner scanner = new java.util.Scanner(input);
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line == null || line.equals("")) {
                continue;
            }
            funcs.add(line);
        }
        scanner.close();

        // get function addresses
        Set<Pair<String, Address>> func_addresses = new HashSet<Pair<String, Address>>();

        for (String func : funcs) {
            Address func_address = null;
            for (Symbol sym : currentProgram.getSymbolTable().getSymbols(func)) {
                func_address = sym.getAddress();
            }
            if (func_address != null) {
                func_addresses.add(new Pair<String, Address>(func, func_address));
            }
        }

        gen_boundary(func_addresses, outputPath);
        gen_stack_boundary(func_addresses, outputPath);
        gen_basic_block(func_addresses, outputPath);
    }

    public void gen_boundary(Set<Pair<String, Address>> func_addresses, String outputPath) throws Exception {
        for (Pair<String, Address> pair : func_addresses) {
            Set<Long> boundary = new HashSet<Long>();
            Function main = currentProgram.getFunctionManager().getFunctionAt(pair.second);
            Iterator<AddressRange> it = main.getBody().iterator();
            // iterate over all the basic blocks
            while (it.hasNext()) {
                AddressRange range = it.next();
                Iterator<Address> ait = range.iterator();
                // iterate over all the instructions in the basic block
                while (ait.hasNext()) {
                    Address addr = ait.next();
                    Instruction ins = currentProgram.getListing().getInstructionContaining(addr);
                    boundary.add(ins.getAddress().getOffset());
                }
            }
            // open filepath ^ "main_boundary"
            File file = new File(outputPath + "/" + pair.first + ".boundary");
            PrintWriter writer = new PrintWriter(file);

            // sort and print hex offset
            boundary.stream().sorted().forEach(x -> writer.println(Long.toHexString(x)));
            writer.close();
        }

    }
    public void gen_stack_boundary(Set<Pair<String, Address>> func_addresses, String outputPath) throws Exception {

        for (Pair<String, Address> pair : func_addresses) {
            Function main = currentProgram.getFunctionManager().getFunctionAt(pair.second);
            int x = 0;
            for (Variable local: main.getStackFrame().getLocals()) {
                x = Math.min(x, local.getStackOffset());
            }
            // open filepath ^ "main_boundary"
            File file = new File(outputPath + "/" + pair.first + ".stack_boundary");
            PrintWriter writer = new PrintWriter(file);

            // sort and print hex offset
            writer.println(String.format("%d", x));
            writer.close();
        }
    }

    public void gen_basic_block(Set<Pair<String, Address>> func_addresses, String outputPath) throws Exception {

        for (Pair<String, Address> pair : func_addresses) {
            Set<Long> boundary = new HashSet<Long>();
            Function main = currentProgram.getFunctionManager().getFunctionAt(pair.second);
            Map<Address, Set<Address>> graph = new HashMap<Address, Set<Address>>();
            BasicBlockModel bbm = new BasicBlockModel(currentProgram);

            AddressSetView body = main.getBody();
            CodeBlockIterator it = bbm.getCodeBlocksContaining(body, monitor);
            while (it.hasNext()) {
                CodeBlock block = it.next();
                CodeBlockReferenceIterator cri = block.getDestinations(monitor);
                while (cri.hasNext()) {
                    CodeBlockReference cbr = cri.next();
                    Address source = cbr.getSourceAddress();
                    Address destination = cbr.getDestinationAddress();
                    if (graph.containsKey(source)) {
                        graph.get(source).add(destination);
                    } else {
                        Set<Address> set = new HashSet<Address>();
                        set.add(destination);
                        graph.put(source, set);
                    }
                }
            }

            
            // open filepath ^ "main_boundary"
            File file = new File(outputPath + "/" + pair.first + ".bb");
            PrintWriter writer = new PrintWriter(file);

            // sort and print hex offset
            List<Address> keySorted = new ArrayList<Address>(graph.keySet());
            keySorted.sort((a, b) -> Long.compare(a.getOffset(),b.getOffset()));
            for (Address source : keySorted) {
                List<Address> valueSorted = new ArrayList<Address>(graph.get(source));
                writer.println(String.format("Block: %x", source.getOffset()));

                valueSorted.sort((a, b) -> Long.compare(a.getOffset(),b.getOffset()));
                writer.print("Successors:");
                // print with space , remove last space
                for (Address destination : valueSorted) {
                    writer.print(String.format(" %x", destination.getOffset()));
                }
                writer.println();
            }
            writer.close();
        }
    }

}