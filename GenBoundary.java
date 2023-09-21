import java.io.File;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

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
        String filepath = getScriptArgs()[0];
        // get main address
        Address main_address = null;
        for (Symbol sym: currentProgram.getSymbolTable().getSymbols("main"))  {
            main_address = sym.getAddress();
        }
        Set<Long> boundary = new HashSet<Long>();
        Function main = currentProgram.getFunctionManager().getFunctionAt(main_address);
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
        File file = new File(filepath + "/main_boundary");
        PrintWriter writer = new PrintWriter(file);

        // sort and print hex offset
        boundary.stream().sorted().forEach(x -> writer.println(Long.toHexString(x)));
        writer.close();
    }
}