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

    public void send_main_address(Socket connected) throws IOException {
        // Get main address
        Address main_address = null;
        for (Symbol sym: currentProgram.getSymbolTable().getSymbols("add"))  {
            main_address = sym.getAddress();
        }
        if (main_address != null) {
            // Write to socket
            byte [] buffer = new byte[8];
            long main_offset = main_address.getOffset();
            for (int i = 0; i < 8; i++) {
                buffer[i] = (byte) (main_offset >> (i * 8));
            }
            println("Main address: " + main_address.toString());
            connected.getOutputStream().write(buffer, 0, 8);
            println("Sent main address");
        } else {
            println("Could not find main address");
            return;
        }

    }

    public void send_address_space(Socket connected) throws IOException {
        // Get address space
        byte [] buffer = new byte[12];
        ByteBuffer byteBuffer = ByteBuffer.wrap(buffer).order(java.nio.ByteOrder.LITTLE_ENDIAN);
        AddressFactory af = currentProgram.getAddressFactory();
        byteBuffer.putInt(af.getUniqueSpace().getSpaceID());
        byteBuffer.putInt(af.getRegisterSpace().getSpaceID());
        byteBuffer.putInt(af.getConstantSpace().getSpaceID());
        // Write to socket
        connected.getOutputStream().write(byteBuffer.array(), byteBuffer.arrayOffset(), byteBuffer.position());
        println("Sent address space");
    }
    

    public void putVarNode(ByteBuffer buffer, Varnode vn) {
        buffer.putInt(vn.getSpace());
        buffer.putLong(vn.getOffset());
        buffer.putInt(vn.getSize());
    }

    public void putPcode(ByteBuffer buffer, PcodeOp op) {
        buffer.putInt(op.getOpcode());
        int numInputs = op.getNumInputs();
        buffer.putInt(numInputs);
        for (int i = 0; i < numInputs; i++) {
            Varnode vn = op.getInput(i);
            putVarNode(buffer, vn);
        }
        if (op.getOutput() == null) {
            buffer.putInt(0);
        } else {
            buffer.putInt(1);
            putVarNode(buffer, op.getOutput());
        }
    }

    public void send_instruction_at(Socket connected, long address, byte[] sendBuffer) throws IOException {
        // Get instruction at address
        Address addr = currentProgram.getAddressFactory().getDefaultAddressSpace().getAddress(address);
        ByteBuffer buffer = ByteBuffer.wrap(sendBuffer).order(java.nio.ByteOrder.LITTLE_ENDIAN);
        Instruction instruction = currentProgram.getListing().getInstructionAt(addr);
        if (instruction == null) {
            buffer.putInt(0);
        }
        else {
            PcodeOp[] pcode = instruction.getPcode();
            int instruction_length = instruction.getLength();
            buffer.putInt(instruction_length);
            buffer.putInt(pcode.length);
            for (int i = 0; i < pcode.length; i++) {
                putPcode(buffer, pcode[i]);
                println(pcode[i].toString());
            }
        }
        // Write to socket
        connected.getOutputStream().write(buffer.array(), buffer.arrayOffset(), buffer.position());
    }

    public void send_data_at(Socket connected, long address, byte[] sendBuffer) throws IOException {
        // Get data at address
        byte [] buffer = new byte[1024];
        // Write to socket
        connected.getOutputStream().write(buffer, 0, 9);
    }

    public void loop_program(Socket connected) throws IOException {
        byte[] sendBuffer = new byte[1024];
        send_main_address(connected);
        send_address_space(connected);
        // Loop until connection is closed
        while (true) {
            // Read from socket
            byte[] recvBuffer = new byte[9];
            int read = connected.getInputStream().read(recvBuffer, 0, 9);
            if (read == -1) {
                break;
            }
            switch (recvBuffer[0]) {
                case 'i': // print instruction
                    // Get address
                    long address = 0;
                    for (int i = 0; i < 8; i++) {
                        address |= (long) (recvBuffer[i + 1] & 0xff) << (i * 8);
                    }
                    send_instruction_at(connected, address, sendBuffer);
                    break;
                case 's': // print data
                    // Get address
                    address = 0;
                    for (int i = 0; i < 8; i++) {
                        address |= (long) (recvBuffer[i + 1] & 0xff) << (i * 8);
                    }
                    send_data_at(connected, address, sendBuffer);
                    break;

            }
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
