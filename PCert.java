import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.List;

import ghidra.program.model.address.Address;
import ghidra.program.model.address.AddressFactory;
import ghidra.program.model.address.AddressSpace;
import ghidra.program.model.lang.Register;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.mem.MemoryAccessException;
import ghidra.program.model.mem.MemoryBlock;
import ghidra.program.model.pcode.PcodeOp;
import ghidra.program.model.pcode.Varnode;
import ghidra.program.model.symbol.Symbol;
import ghidra.app.plugin.processors.sleigh.SleighLanguage;
import ghidra.app.plugin.processors.sleigh.symbol.SymbolTable;
import ghidra.app.plugin.core.analysis.AutoAnalysisManager;
import ghidra.app.script.GhidraScript;
import ghidra.framework.options.OptionType;
import ghidra.framework.options.Options;

public class PCert extends GhidraScript {

    public boolean debug = false;

    public void println(String s) {
        if (debug) {
            super.println(s);
        }
    }

    public void send_address_space(DataOutputStream out) throws IOException {
        // Get address space
        AddressFactory af = currentProgram.getAddressFactory();
        out.writeInt(af.getUniqueSpace().getSpaceID());
        out.writeInt(af.getRegisterSpace().getSpaceID());
        out.writeInt(af.getConstantSpace().getSpaceID());
        out.writeInt(af.getDefaultAddressSpace().getSpaceID());
        // Write to socket
        println("Sent address space");
    }

    public void putVarNode(DataOutputStream out, Varnode vn) throws IOException {
        out.writeInt(vn.getSpace());
        out.writeLong(vn.getOffset());
        out.writeInt(vn.getSize());
    }

    public void writeString(DataOutputStream out, String s) throws IOException {
        out.writeInt(s.length());
        out.writeBytes(s);
    }

    public void putPcode(DataOutputStream out, String mnem, PcodeOp op) throws IOException {
        writeString(out, mnem);
        out.writeInt(op.getOpcode());
        int numInputs = op.getNumInputs();
        out.writeInt(numInputs);
        for (int i = 0; i < numInputs; i++) {
            Varnode vn = op.getInput(i);
            putVarNode(out, vn);
        }
        if (op.getOutput() == null) {
            out.writeInt(0);
        } else {
            out.writeInt(1);
            putVarNode(out, op.getOutput());
        }
    }

    public void send_instruction_at(DataOutputStream out, long address) throws IOException {
        // Get instruction at address
        Address addr = currentProgram.getAddressFactory().getDefaultAddressSpace().getAddress(address);
        Instruction instruction = currentProgram.getListing().getInstructionAt(addr);
        if (instruction == null) {
            disassemble(addr);
            instruction = currentProgram.getListing().getInstructionAt(addr);
        }
        if (instruction == null) {
            out.writeInt(0);
        } else {
            SymbolTable symbolTable = ((SleighLanguage) currentProgram.getLanguage()).getSymbolTable();
            PcodeOp[] pcode = instruction.getPcode();
            int instruction_length = instruction.getLength();
            out.writeInt(instruction_length);
            out.writeInt(pcode.length);
            for (int i = 0; i < pcode.length; i++) {
                String mnemonic = instruction.getMnemonicString();
                String extra = "0";
                if (pcode[i].getOpcode() == PcodeOp.CALLOTHER) {
                    extra = symbolTable.getUserDefinedOpName((int) pcode[i].getInput(0).getOffset());
                }
                putPcode(out, mnemonic + ":" + extra, pcode[i]);
                println(pcode[i].toString());
            }
        }
    }

    public void send_data_at(DataOutputStream out, long address) throws IOException {
        byte[] data = new byte[1];
        try {
            currentProgram.getMemory()
                    .getBytes(currentProgram.getAddressFactory().getDefaultAddressSpace().getAddress(address), data);
        } catch (Exception e) {
            out.writeByte(0);
            return;
        }
        byte data_byte = ByteBuffer.wrap(data).order(java.nio.ByteOrder.LITTLE_ENDIAN).get();
        // Write to socket
        out.writeByte(data_byte);
    }

    public void dump_memory(DataOutputStream out, String name) throws IOException, MemoryAccessException {
        AddressSpace space = currentProgram.getAddressFactory().getDefaultAddressSpace();
        File file = new File(name);
        DataOutputStream file_out = new DataOutputStream(new BufferedOutputStream(new java.io.FileOutputStream(file)));

        MemoryBlock[] blocks = currentProgram.getMemory().getBlocks();
        List<MemoryBlock> block_list = new java.util.ArrayList<MemoryBlock>();
        for (MemoryBlock block : blocks) {
            if (block.getStart().getAddressSpace().equals(space) && block.isInitialized()) {
                block_list.add(block);
            }
        }
        file_out.writeInt(block_list.size());
        for (MemoryBlock block : block_list) {
            file_out.writeLong(block.getStart().getOffset());
            byte[] data = new byte[(int) block.getSize()];
            file_out.writeInt(data.length);
            block.getBytes(block.getStart(), data);
            file_out.write(data);
        }
        file_out.close();
        out.writeLong(file.length());
    }

    public void send_function_addr(DataOutputStream out, String name) throws IOException {
        Address func_address = null;
        for (Symbol sym : currentProgram.getSymbolTable().getSymbols(name)) {
            func_address = sym.getAddress();
        }
        if (func_address != null) {
            out.writeLong(func_address.getOffset());
        } else {
            out.writeLong(-1);
        }
    }

    public void add_thunk_to(Address addr, List<Address> func_addresses) {
        if (func_addresses.contains(addr)) {
            return;
        }
        func_addresses.add(addr);
        Function func = currentProgram.getFunctionManager().getFunctionAt(addr);
        if (func == null) {
            return;
        }
        Address[] thunk_addrs = func.getFunctionThunkAddresses(true);
        if (thunk_addrs == null) {
            return;
        }
        for (Address thunk_addr : thunk_addrs) {
            add_thunk_to(thunk_addr, func_addresses);
        }
    }

    public void send_register_number(DataOutputStream out) throws IOException {
        List<Register> rs = currentProgram.getProgramContext().getRegisters();
        List<Register> brs = rs.stream().filter(r -> r.getBaseRegister().getName().equals(r.getName())).toList();
        out.writeInt(brs.size());
        for (Register r : brs) {
            out.writeInt(r.getOffset());
            out.writeInt(r.getBitLength() / 8);
        }
        out.writeInt(rs.size());
        for (Register r : rs) {
            writeString(out, r.getName());
            out.writeInt(r.getBaseRegister().getOffset());
            out.writeInt(r.getOffset() - r.getBaseRegister().getOffset());
            out.writeInt(r.getBitLength() / 8);
        }
    }

    public void send_external_functions(DataOutputStream out) throws IOException {
        List<Function> ex_funcs = new java.util.ArrayList<Function>();
        for (Function func : currentProgram.getFunctionManager().getExternalFunctions()) {
            ex_funcs.add(func);
        }
        out.writeInt(ex_funcs.size());
        // (y2 - y1) * (x3 - x1) / (x2 - x1) + y1 to find PLT address
        Long x1 = 0L;
        Long y1 = 0L;
        Long x2 = 0L;
        Long y2 = 0L;
        int count = 0;
        for (Function func : ex_funcs) {
            List<Address> func_addresses = new java.util.ArrayList<Address>();
            for (Address thunk_addr : func.getFunctionThunkAddresses(true)) {
                add_thunk_to(thunk_addr, func_addresses);
            }
            if (func_addresses.size() == 2) {
                if (count == 0) {
                    x1 = func_addresses.get(0).getOffset();
                    y1 = func_addresses.get(1).getOffset();
                } else if (count == 1) {
                    x2 = func_addresses.get(0).getOffset();
                    y2 = func_addresses.get(1).getOffset();
                } else {
                    break;
                }
                count++;
            }
        }
        for (Function func : ex_funcs) {
            List<Address> func_addresses = new java.util.ArrayList<Address>();
            for (Address thunk_addr : func.getFunctionThunkAddresses(true)) {
                add_thunk_to(thunk_addr, func_addresses);
            }
            if (func_addresses.size() == 1 && count >= 2) {
                long x3 = func_addresses.get(0).getOffset();
                long y3 = (y2 - y1) * (x3 - x1) / (x2 - x1) + y1;
                func_addresses.add(currentProgram.getAddressFactory().getDefaultAddressSpace().getAddress(y3));
            }

            writeString(out, func.getName());
            out.writeInt(func_addresses.size());
            for (Address addr : func_addresses) {
                out.writeLong(addr.getOffset());
            }
        }
    }

    public void loop_program(Socket connected) throws IOException, MemoryAccessException {
        byte[] nameBuffer;
        String name;
        DataInputStream in = new DataInputStream(connected.getInputStream());
        DataOutputStream out = new DataOutputStream(new BufferedOutputStream(connected.getOutputStream()));
        send_address_space(out);
        send_register_number(out);
        send_external_functions(out);
        out.flush();
        // Loop until connection is closed
        // Read from socket
        while (true) {
            switch (in.readByte()) {
                case 'i': // print instruction
                    // Get address
                    long address = in.readLong();
                    send_instruction_at(out, address);
                    break;
                case 's': // print data
                    // Get address
                    address = in.readLong();
                    send_data_at(out, address);
                    break;
                case 'f': // get function address
                    // println("Getting function address");
                    int size = in.readInt();
                    nameBuffer = new byte[size];
                    in.read(nameBuffer, 0, size);
                    name = new String(nameBuffer);
                    send_function_addr(out, name);
                    break;
                case 'd': // dump memory
                    size = in.readInt();
                    nameBuffer = new byte[size];
                    in.read(nameBuffer, 0, size);
                    name = new String(nameBuffer);
                    dump_memory(out, name);
                    break;
            }
            out.flush();
        }
    }

    void initializeRef() {
        Options o = currentProgram.getOptions("Analyzers");
        for (Options sub : o.getChildOptions()) {
            if (o.getType(sub.getName()) == OptionType.BOOLEAN_TYPE) {
                o.setBoolean(sub.getName(), false);
            }
        }
        o.setBoolean("Function Start Search", true);
        o.setBoolean("Reference", true);
        o.setBoolean("Subroutine References", true);
        AutoAnalysisManager mgr = AutoAnalysisManager.getAnalysisManager(currentProgram);
        mgr.initializeOptions();
        mgr.reAnalyzeAll(null);
        mgr.startAnalysis(monitor);
    }

    @Override
    protected void run() throws Exception {
        // get arguments and print
        initializeRef();
        String[] args = getScriptArgs();
        if (args.length != 1) {
            println("Usage: PCert <arg>");
            return;
        }
        int port = Integer.parseInt(args[0]);
        println("Port: " + port);
        Socket clientSocket;
        // Listen on port
        try {
            clientSocket = new Socket("localhost", port);
            loop_program(clientSocket);
            clientSocket.close();
        } catch (IOException e) {
            println("Could not listen on port " + port);
            return;
        }
    }

}
