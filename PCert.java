import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.ByteBuffer;

import ghidra.program.model.address.Address;
import ghidra.program.model.address.AddressFactory;
import ghidra.program.model.address.AddressSpace;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.pcode.PcodeOp;
import ghidra.program.model.pcode.Varnode;
import ghidra.program.model.symbol.Symbol;
import ghidra.app.script.GhidraScript;

public class PCert extends GhidraScript {

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

    public void putPcode(DataOutputStream out, PcodeOp op) throws IOException {
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
            out.writeInt(0);
        }
        else {
            PcodeOp[] pcode = instruction.getPcode();
            int instruction_length = instruction.getLength();
            out.writeInt(instruction_length);
            out.writeInt(pcode.length);
            for (int i = 0; i < pcode.length; i++) {
                putPcode(out, pcode[i]);
                println(pcode[i].toString());
            }
        }
    }

    public void send_data_at(DataOutputStream out, long address) throws IOException {
        byte [] data = new byte[8];
        try {
            currentProgram.getMemory().getBytes(currentProgram.getAddressFactory().getDefaultAddressSpace().getAddress(address), data);
        } catch (Exception e) {
            out.writeLong(-1);
            return;
        }
        long data_long = ByteBuffer.wrap(data).order(java.nio.ByteOrder.LITTLE_ENDIAN).getLong();
        // Write to socket
        out.writeLong(data_long);
    }

    public void send_function_addr(DataOutputStream out, String name) throws IOException {
        Address func_address = null;
        for (Symbol sym: currentProgram.getSymbolTable().getSymbols(name))  {
            func_address = sym.getAddress();
        }
        if (func_address != null) {
            out.writeLong(func_address.getOffset());
        } else {
            out.writeLong(-1);
        }
    }

    public void loop_program(Socket connected) throws IOException {
        DataInputStream in = new DataInputStream(connected.getInputStream());
        DataOutputStream out = new DataOutputStream(new BufferedOutputStream(connected.getOutputStream()));
        send_address_space(out);
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
                    println("Getting function address");
                    int size = in.readInt();
                    byte[] nameBuffer = new byte[size];
                    in.read(nameBuffer, 0, size);
                    String name = new String(nameBuffer);
                    send_function_addr(out, name);
                    break;
            }
            out.flush();
        }
    }

    @Override
    protected void run() throws Exception {
        // get arguments and print
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
