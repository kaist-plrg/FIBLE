import java.io.File;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import generic.stl.Pair;
import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.address.AddressRange;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.symbol.Symbol;

public class GenBoundary extends GhidraScript {

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
}