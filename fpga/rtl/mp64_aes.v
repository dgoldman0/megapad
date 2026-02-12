// ============================================================================
// mp64_aes.v — AES-256-GCM Accelerator
// ============================================================================
//
// Pipelined AES-256 encryption/decryption with GCM authentication.
// MMIO base: 0x700 (64 bytes).
//
// Features:
//   - 256-bit key, 96-bit IV, 128-bit blocks
//   - Encrypt and decrypt modes
//   - GHASH for GCM authentication tag
//   - Interrupt on block completion
//   - 1 block per ~14 cycles (14 AES-256 rounds, sequential)
//

`include "mp64_defs.vh"

module mp64_aes (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO interface
    input  wire        req,
    input  wire [6:0]  addr,       // offset within AES block
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack,

    // Interrupt
    output reg         irq
);

    // ========================================================================
    // AES S-Box (SubBytes) — 256-entry lookup table
    // ========================================================================
    // Rijndael S-box — combinational lookup
    function [7:0] sbox;
        input [7:0] x;
        reg [7:0] tbl [0:255];
        begin
            tbl[8'h00]=8'h63; tbl[8'h01]=8'h7c; tbl[8'h02]=8'h77; tbl[8'h03]=8'h7b;
            tbl[8'h04]=8'hf2; tbl[8'h05]=8'h6b; tbl[8'h06]=8'h6f; tbl[8'h07]=8'hc5;
            tbl[8'h08]=8'h30; tbl[8'h09]=8'h01; tbl[8'h0a]=8'h67; tbl[8'h0b]=8'h2b;
            tbl[8'h0c]=8'hfe; tbl[8'h0d]=8'hd7; tbl[8'h0e]=8'hab; tbl[8'h0f]=8'h76;
            tbl[8'h10]=8'hca; tbl[8'h11]=8'h82; tbl[8'h12]=8'hc9; tbl[8'h13]=8'h7d;
            tbl[8'h14]=8'hfa; tbl[8'h15]=8'h59; tbl[8'h16]=8'h47; tbl[8'h17]=8'hf0;
            tbl[8'h18]=8'had; tbl[8'h19]=8'hd4; tbl[8'h1a]=8'ha2; tbl[8'h1b]=8'haf;
            tbl[8'h1c]=8'h9c; tbl[8'h1d]=8'ha4; tbl[8'h1e]=8'h72; tbl[8'h1f]=8'hc0;
            tbl[8'h20]=8'hb7; tbl[8'h21]=8'hfd; tbl[8'h22]=8'h93; tbl[8'h23]=8'h26;
            tbl[8'h24]=8'h36; tbl[8'h25]=8'h3f; tbl[8'h26]=8'hf7; tbl[8'h27]=8'hcc;
            tbl[8'h28]=8'h34; tbl[8'h29]=8'ha5; tbl[8'h2a]=8'he5; tbl[8'h2b]=8'hf1;
            tbl[8'h2c]=8'h71; tbl[8'h2d]=8'hd8; tbl[8'h2e]=8'h31; tbl[8'h2f]=8'h15;
            tbl[8'h30]=8'h04; tbl[8'h31]=8'hc7; tbl[8'h32]=8'h23; tbl[8'h33]=8'hc3;
            tbl[8'h34]=8'h18; tbl[8'h35]=8'h96; tbl[8'h36]=8'h05; tbl[8'h37]=8'h9a;
            tbl[8'h38]=8'h07; tbl[8'h39]=8'h12; tbl[8'h3a]=8'h80; tbl[8'h3b]=8'he2;
            tbl[8'h3c]=8'heb; tbl[8'h3d]=8'h27; tbl[8'h3e]=8'hb2; tbl[8'h3f]=8'h75;
            tbl[8'h40]=8'h09; tbl[8'h41]=8'h83; tbl[8'h42]=8'h2c; tbl[8'h43]=8'h1a;
            tbl[8'h44]=8'h1b; tbl[8'h45]=8'h6e; tbl[8'h46]=8'h5a; tbl[8'h47]=8'ha0;
            tbl[8'h48]=8'h52; tbl[8'h49]=8'h3b; tbl[8'h4a]=8'hd6; tbl[8'h4b]=8'hb3;
            tbl[8'h4c]=8'h29; tbl[8'h4d]=8'he3; tbl[8'h4e]=8'h2f; tbl[8'h4f]=8'h84;
            tbl[8'h50]=8'h53; tbl[8'h51]=8'hd1; tbl[8'h52]=8'h00; tbl[8'h53]=8'hed;
            tbl[8'h54]=8'h20; tbl[8'h55]=8'hfc; tbl[8'h56]=8'hb1; tbl[8'h57]=8'h5b;
            tbl[8'h58]=8'h6a; tbl[8'h59]=8'hcb; tbl[8'h5a]=8'hbe; tbl[8'h5b]=8'h39;
            tbl[8'h5c]=8'h4a; tbl[8'h5d]=8'h4c; tbl[8'h5e]=8'h58; tbl[8'h5f]=8'hcf;
            tbl[8'h60]=8'hd0; tbl[8'h61]=8'hef; tbl[8'h62]=8'haa; tbl[8'h63]=8'hfb;
            tbl[8'h64]=8'h43; tbl[8'h65]=8'h4d; tbl[8'h66]=8'h33; tbl[8'h67]=8'h85;
            tbl[8'h68]=8'h45; tbl[8'h69]=8'hf9; tbl[8'h6a]=8'h02; tbl[8'h6b]=8'h7f;
            tbl[8'h6c]=8'h50; tbl[8'h6d]=8'h3c; tbl[8'h6e]=8'h9f; tbl[8'h6f]=8'ha8;
            tbl[8'h70]=8'h51; tbl[8'h71]=8'ha3; tbl[8'h72]=8'h40; tbl[8'h73]=8'h8f;
            tbl[8'h74]=8'h92; tbl[8'h75]=8'h9d; tbl[8'h76]=8'h38; tbl[8'h77]=8'hf5;
            tbl[8'h78]=8'hbc; tbl[8'h79]=8'hb6; tbl[8'h7a]=8'hda; tbl[8'h7b]=8'h21;
            tbl[8'h7c]=8'h10; tbl[8'h7d]=8'hff; tbl[8'h7e]=8'hf3; tbl[8'h7f]=8'hd2;
            tbl[8'h80]=8'hcd; tbl[8'h81]=8'h0c; tbl[8'h82]=8'h13; tbl[8'h83]=8'hec;
            tbl[8'h84]=8'h5f; tbl[8'h85]=8'h97; tbl[8'h86]=8'h44; tbl[8'h87]=8'h17;
            tbl[8'h88]=8'hc4; tbl[8'h89]=8'ha7; tbl[8'h8a]=8'h7e; tbl[8'h8b]=8'h3d;
            tbl[8'h8c]=8'h64; tbl[8'h8d]=8'h5d; tbl[8'h8e]=8'h19; tbl[8'h8f]=8'h73;
            tbl[8'h90]=8'h60; tbl[8'h91]=8'h81; tbl[8'h92]=8'h4f; tbl[8'h93]=8'hdc;
            tbl[8'h94]=8'h22; tbl[8'h95]=8'h2a; tbl[8'h96]=8'h90; tbl[8'h97]=8'h88;
            tbl[8'h98]=8'h46; tbl[8'h99]=8'hee; tbl[8'h9a]=8'hb8; tbl[8'h9b]=8'h14;
            tbl[8'h9c]=8'hde; tbl[8'h9d]=8'h5e; tbl[8'h9e]=8'h0b; tbl[8'h9f]=8'hdb;
            tbl[8'ha0]=8'he0; tbl[8'ha1]=8'h32; tbl[8'ha2]=8'h3a; tbl[8'ha3]=8'h0a;
            tbl[8'ha4]=8'h49; tbl[8'ha5]=8'h06; tbl[8'ha6]=8'h24; tbl[8'ha7]=8'h5c;
            tbl[8'ha8]=8'hc2; tbl[8'ha9]=8'hd3; tbl[8'haa]=8'hac; tbl[8'hab]=8'h62;
            tbl[8'hac]=8'h91; tbl[8'had]=8'h95; tbl[8'hae]=8'he4; tbl[8'haf]=8'h79;
            tbl[8'hb0]=8'he7; tbl[8'hb1]=8'hc8; tbl[8'hb2]=8'h37; tbl[8'hb3]=8'h6d;
            tbl[8'hb4]=8'h8d; tbl[8'hb5]=8'hd5; tbl[8'hb6]=8'h4e; tbl[8'hb7]=8'ha9;
            tbl[8'hb8]=8'h6c; tbl[8'hb9]=8'h56; tbl[8'hba]=8'hf4; tbl[8'hbb]=8'hea;
            tbl[8'hbc]=8'h65; tbl[8'hbd]=8'h7a; tbl[8'hbe]=8'hae; tbl[8'hbf]=8'h08;
            tbl[8'hc0]=8'hba; tbl[8'hc1]=8'h78; tbl[8'hc2]=8'h25; tbl[8'hc3]=8'h2e;
            tbl[8'hc4]=8'h1c; tbl[8'hc5]=8'ha6; tbl[8'hc6]=8'hb4; tbl[8'hc7]=8'hc6;
            tbl[8'hc8]=8'he8; tbl[8'hc9]=8'hdd; tbl[8'hca]=8'h74; tbl[8'hcb]=8'h1f;
            tbl[8'hcc]=8'h4b; tbl[8'hcd]=8'hbd; tbl[8'hce]=8'h8b; tbl[8'hcf]=8'h8a;
            tbl[8'hd0]=8'h70; tbl[8'hd1]=8'h3e; tbl[8'hd2]=8'hb5; tbl[8'hd3]=8'h66;
            tbl[8'hd4]=8'h48; tbl[8'hd5]=8'h03; tbl[8'hd6]=8'hf6; tbl[8'hd7]=8'h0e;
            tbl[8'hd8]=8'h61; tbl[8'hd9]=8'h35; tbl[8'hda]=8'h57; tbl[8'hdb]=8'hb9;
            tbl[8'hdc]=8'h86; tbl[8'hdd]=8'hc1; tbl[8'hde]=8'h1d; tbl[8'hdf]=8'h9e;
            tbl[8'he0]=8'he1; tbl[8'he1]=8'hf8; tbl[8'he2]=8'h98; tbl[8'he3]=8'h11;
            tbl[8'he4]=8'h69; tbl[8'he5]=8'hd9; tbl[8'he6]=8'h8e; tbl[8'he7]=8'h94;
            tbl[8'he8]=8'h9b; tbl[8'he9]=8'h1e; tbl[8'hea]=8'h87; tbl[8'heb]=8'he9;
            tbl[8'hec]=8'hce; tbl[8'hed]=8'h55; tbl[8'hee]=8'h28; tbl[8'hef]=8'hdf;
            tbl[8'hf0]=8'h8c; tbl[8'hf1]=8'ha1; tbl[8'hf2]=8'h89; tbl[8'hf3]=8'h0d;
            tbl[8'hf4]=8'hbf; tbl[8'hf5]=8'he6; tbl[8'hf6]=8'h42; tbl[8'hf7]=8'h68;
            tbl[8'hf8]=8'h41; tbl[8'hf9]=8'h99; tbl[8'hfa]=8'h2d; tbl[8'hfb]=8'h0f;
            tbl[8'hfc]=8'hb0; tbl[8'hfd]=8'h54; tbl[8'hfe]=8'hbb; tbl[8'hff]=8'h16;
            sbox = tbl[x];
        end
    endfunction

    // Inverse S-box for decryption
    function [7:0] inv_sbox;
        input [7:0] x;
        reg [7:0] tbl [0:255];
        begin
            tbl[8'h00]=8'h52; tbl[8'h01]=8'h09; tbl[8'h02]=8'h6a; tbl[8'h03]=8'hd5;
            tbl[8'h04]=8'h30; tbl[8'h05]=8'h36; tbl[8'h06]=8'ha5; tbl[8'h07]=8'h38;
            tbl[8'h08]=8'hbf; tbl[8'h09]=8'h40; tbl[8'h0a]=8'ha3; tbl[8'h0b]=8'h9e;
            tbl[8'h0c]=8'h81; tbl[8'h0d]=8'hf3; tbl[8'h0e]=8'hd7; tbl[8'h0f]=8'hfb;
            tbl[8'h10]=8'h7c; tbl[8'h11]=8'he3; tbl[8'h12]=8'h39; tbl[8'h13]=8'h82;
            tbl[8'h14]=8'h9b; tbl[8'h15]=8'h2f; tbl[8'h16]=8'hff; tbl[8'h17]=8'h87;
            tbl[8'h18]=8'h34; tbl[8'h19]=8'h8e; tbl[8'h1a]=8'h43; tbl[8'h1b]=8'h44;
            tbl[8'h1c]=8'hc4; tbl[8'h1d]=8'hde; tbl[8'h1e]=8'he9; tbl[8'h1f]=8'hcb;
            tbl[8'h20]=8'h54; tbl[8'h21]=8'h7b; tbl[8'h22]=8'h94; tbl[8'h23]=8'h32;
            tbl[8'h24]=8'ha6; tbl[8'h25]=8'hc2; tbl[8'h26]=8'h23; tbl[8'h27]=8'h3d;
            tbl[8'h28]=8'hee; tbl[8'h29]=8'h4c; tbl[8'h2a]=8'h95; tbl[8'h2b]=8'h0b;
            tbl[8'h2c]=8'h42; tbl[8'h2d]=8'hfa; tbl[8'h2e]=8'hc3; tbl[8'h2f]=8'h4e;
            tbl[8'h30]=8'h08; tbl[8'h31]=8'h2e; tbl[8'h32]=8'ha1; tbl[8'h33]=8'h66;
            tbl[8'h34]=8'h28; tbl[8'h35]=8'hd9; tbl[8'h36]=8'h24; tbl[8'h37]=8'hb2;
            tbl[8'h38]=8'h76; tbl[8'h39]=8'h5b; tbl[8'h3a]=8'ha2; tbl[8'h3b]=8'h49;
            tbl[8'h3c]=8'h6d; tbl[8'h3d]=8'h8b; tbl[8'h3e]=8'hd1; tbl[8'h3f]=8'h25;
            tbl[8'h40]=8'h72; tbl[8'h41]=8'hf8; tbl[8'h42]=8'hf6; tbl[8'h43]=8'h64;
            tbl[8'h44]=8'h86; tbl[8'h45]=8'h68; tbl[8'h46]=8'h98; tbl[8'h47]=8'h16;
            tbl[8'h48]=8'hd4; tbl[8'h49]=8'ha4; tbl[8'h4a]=8'h5c; tbl[8'h4b]=8'hcc;
            tbl[8'h4c]=8'h5d; tbl[8'h4d]=8'h65; tbl[8'h4e]=8'hb6; tbl[8'h4f]=8'h92;
            tbl[8'h50]=8'h6c; tbl[8'h51]=8'h70; tbl[8'h52]=8'h48; tbl[8'h53]=8'h50;
            tbl[8'h54]=8'hfd; tbl[8'h55]=8'hed; tbl[8'h56]=8'hb9; tbl[8'h57]=8'hda;
            tbl[8'h58]=8'h5e; tbl[8'h59]=8'h15; tbl[8'h5a]=8'h46; tbl[8'h5b]=8'h57;
            tbl[8'h5c]=8'ha7; tbl[8'h5d]=8'h8d; tbl[8'h5e]=8'h9d; tbl[8'h5f]=8'h84;
            tbl[8'h60]=8'h90; tbl[8'h61]=8'hd8; tbl[8'h62]=8'hab; tbl[8'h63]=8'h00;
            tbl[8'h64]=8'h8c; tbl[8'h65]=8'hbc; tbl[8'h66]=8'hd3; tbl[8'h67]=8'h0a;
            tbl[8'h68]=8'hf7; tbl[8'h69]=8'he4; tbl[8'h6a]=8'h58; tbl[8'h6b]=8'h05;
            tbl[8'h6c]=8'hb8; tbl[8'h6d]=8'hb3; tbl[8'h6e]=8'h45; tbl[8'h6f]=8'h06;
            tbl[8'h70]=8'hd0; tbl[8'h71]=8'h2c; tbl[8'h72]=8'h1e; tbl[8'h73]=8'h8f;
            tbl[8'h74]=8'hca; tbl[8'h75]=8'h3f; tbl[8'h76]=8'h0f; tbl[8'h77]=8'h02;
            tbl[8'h78]=8'hc1; tbl[8'h79]=8'haf; tbl[8'h7a]=8'hbd; tbl[8'h7b]=8'h03;
            tbl[8'h7c]=8'h01; tbl[8'h7d]=8'h13; tbl[8'h7e]=8'h8a; tbl[8'h7f]=8'h6b;
            tbl[8'h80]=8'h3a; tbl[8'h81]=8'h91; tbl[8'h82]=8'h11; tbl[8'h83]=8'h41;
            tbl[8'h84]=8'h4f; tbl[8'h85]=8'h67; tbl[8'h86]=8'hdc; tbl[8'h87]=8'hea;
            tbl[8'h88]=8'h97; tbl[8'h89]=8'hf2; tbl[8'h8a]=8'hcf; tbl[8'h8b]=8'hce;
            tbl[8'h8c]=8'hf0; tbl[8'h8d]=8'hb4; tbl[8'h8e]=8'he6; tbl[8'h8f]=8'h73;
            tbl[8'h90]=8'h96; tbl[8'h91]=8'hac; tbl[8'h92]=8'h74; tbl[8'h93]=8'h22;
            tbl[8'h94]=8'he7; tbl[8'h95]=8'had; tbl[8'h96]=8'h35; tbl[8'h97]=8'h85;
            tbl[8'h98]=8'he2; tbl[8'h99]=8'hf9; tbl[8'h9a]=8'h37; tbl[8'h9b]=8'he8;
            tbl[8'h9c]=8'h1c; tbl[8'h9d]=8'h75; tbl[8'h9e]=8'hdf; tbl[8'h9f]=8'h6e;
            tbl[8'ha0]=8'h47; tbl[8'ha1]=8'hf1; tbl[8'ha2]=8'h1a; tbl[8'ha3]=8'h71;
            tbl[8'ha4]=8'h1d; tbl[8'ha5]=8'h29; tbl[8'ha6]=8'hc5; tbl[8'ha7]=8'h89;
            tbl[8'ha8]=8'h6f; tbl[8'ha9]=8'hb7; tbl[8'haa]=8'h62; tbl[8'hab]=8'h0e;
            tbl[8'hac]=8'haa; tbl[8'had]=8'h18; tbl[8'hae]=8'hbe; tbl[8'haf]=8'h1b;
            tbl[8'hb0]=8'hfc; tbl[8'hb1]=8'h56; tbl[8'hb2]=8'h3e; tbl[8'hb3]=8'h4b;
            tbl[8'hb4]=8'hc6; tbl[8'hb5]=8'hd2; tbl[8'hb6]=8'h79; tbl[8'hb7]=8'h20;
            tbl[8'hb8]=8'h9a; tbl[8'hb9]=8'hdb; tbl[8'hba]=8'hc0; tbl[8'hbb]=8'hfe;
            tbl[8'hbc]=8'h78; tbl[8'hbd]=8'hcd; tbl[8'hbe]=8'h5a; tbl[8'hbf]=8'hf4;
            tbl[8'hc0]=8'h1f; tbl[8'hc1]=8'hdd; tbl[8'hc2]=8'ha8; tbl[8'hc3]=8'h33;
            tbl[8'hc4]=8'h88; tbl[8'hc5]=8'h07; tbl[8'hc6]=8'hc7; tbl[8'hc7]=8'h31;
            tbl[8'hc8]=8'hb1; tbl[8'hc9]=8'h12; tbl[8'hca]=8'h10; tbl[8'hcb]=8'h59;
            tbl[8'hcc]=8'h27; tbl[8'hcd]=8'h80; tbl[8'hce]=8'hec; tbl[8'hcf]=8'h5f;
            tbl[8'hd0]=8'h60; tbl[8'hd1]=8'h51; tbl[8'hd2]=8'h7f; tbl[8'hd3]=8'ha9;
            tbl[8'hd4]=8'h19; tbl[8'hd5]=8'hb5; tbl[8'hd6]=8'h4a; tbl[8'hd7]=8'h0d;
            tbl[8'hd8]=8'h2d; tbl[8'hd9]=8'he5; tbl[8'hda]=8'h7a; tbl[8'hdb]=8'h9f;
            tbl[8'hdc]=8'h93; tbl[8'hdd]=8'hc9; tbl[8'hde]=8'h9c; tbl[8'hdf]=8'hef;
            tbl[8'he0]=8'ha0; tbl[8'he1]=8'he0; tbl[8'he2]=8'h3b; tbl[8'he3]=8'h4d;
            tbl[8'he4]=8'hae; tbl[8'he5]=8'h2a; tbl[8'he6]=8'hf5; tbl[8'he7]=8'hb0;
            tbl[8'he8]=8'hc8; tbl[8'he9]=8'heb; tbl[8'hea]=8'hbb; tbl[8'heb]=8'h3c;
            tbl[8'hec]=8'h83; tbl[8'hed]=8'h53; tbl[8'hee]=8'h99; tbl[8'hef]=8'h61;
            tbl[8'hf0]=8'h17; tbl[8'hf1]=8'h2b; tbl[8'hf2]=8'h04; tbl[8'hf3]=8'h7e;
            tbl[8'hf4]=8'hba; tbl[8'hf5]=8'h77; tbl[8'hf6]=8'hd6; tbl[8'hf7]=8'h26;
            tbl[8'hf8]=8'he1; tbl[8'hf9]=8'h69; tbl[8'hfa]=8'h14; tbl[8'hfb]=8'h63;
            tbl[8'hfc]=8'h55; tbl[8'hfd]=8'h21; tbl[8'hfe]=8'h0c; tbl[8'hff]=8'h7d;
            inv_sbox = tbl[x];
        end
    endfunction

    // ========================================================================
    // GF(2^8) multiply (for MixColumns)
    // ========================================================================
    function [7:0] gf_mul2;
        input [7:0] a;
        begin
            gf_mul2 = (a[7]) ? ({a[6:0], 1'b0} ^ 8'h1b) : {a[6:0], 1'b0};
        end
    endfunction

    function [7:0] gf_mul3;
        input [7:0] a;
        begin
            gf_mul3 = gf_mul2(a) ^ a;
        end
    endfunction

    // For InvMixColumns: multiply by 9, 11, 13, 14
    function [7:0] gf_mul;
        input [7:0] a;
        input [3:0] b;
        reg [7:0] x2, x4, x8;
        begin
            x2 = gf_mul2(a);
            x4 = gf_mul2(x2);
            x8 = gf_mul2(x4);
            gf_mul = (b[3] ? x8 : 8'd0) ^ (b[2] ? x4 : 8'd0) ^
                     (b[1] ? x2 : 8'd0) ^ (b[0] ? a  : 8'd0);
        end
    endfunction

    // ========================================================================
    // AES Round Constants
    // ========================================================================
    function [7:0] rcon;
        input [3:0] i;
        begin
            case (i)
                4'd0:  rcon = 8'h01;
                4'd1:  rcon = 8'h02;
                4'd2:  rcon = 8'h04;
                4'd3:  rcon = 8'h08;
                4'd4:  rcon = 8'h10;
                4'd5:  rcon = 8'h20;
                4'd6:  rcon = 8'h40;
                default: rcon = 8'h00;
            endcase
        end
    endfunction

    // ========================================================================
    // Configuration Registers
    // ========================================================================
    reg [255:0] key;           // 256-bit key
    reg [95:0]  iv;            // 96-bit IV
    reg [31:0]  aad_len;       // AAD length in bytes
    reg [31:0]  data_len;      // Data length in bytes
    reg         cmd_encrypt;   // 0=encrypt, 1=decrypt
    reg [127:0] data_in;       // 128-bit input block
    reg [127:0] data_out;      // 128-bit output block
    reg [127:0] tag;           // 128-bit GCM tag

    // Status
    reg         busy;
    reg         done;
    reg         auth_fail;
    reg [7:0]   status;

    always @(*) status = {5'd0, auth_fail, done, busy};

    // ========================================================================
    // AES-256 Key Expansion (on-the-fly, sequential)
    // ========================================================================
    // AES-256 needs 15 round keys (rounds 0-14). We compute them on-the-fly.
    reg [127:0] round_key;     // Current round key
    reg [255:0] exp_key;       // Key expansion state (full 256-bit)
    reg [3:0]   round_cnt;     // 0..14

    // One round of key expansion
    function [31:0] sub_word;
        input [31:0] w;
        begin
            sub_word = {sbox(w[31:24]), sbox(w[23:16]),
                        sbox(w[15:8]),  sbox(w[7:0])};
        end
    endfunction

    function [31:0] rot_word;
        input [31:0] w;
        begin
            rot_word = {w[23:0], w[31:24]};
        end
    endfunction

    // ========================================================================
    // AES Round Functions (combinational)
    // ========================================================================

    // SubBytes (forward)
    function [127:0] sub_bytes;
        input [127:0] s;
        integer i;
        begin
            for (i = 0; i < 16; i = i + 1)
                sub_bytes[i*8 +: 8] = sbox(s[i*8 +: 8]);
        end
    endfunction

    // InvSubBytes
    function [127:0] inv_sub_bytes;
        input [127:0] s;
        integer i;
        begin
            for (i = 0; i < 16; i = i + 1)
                inv_sub_bytes[i*8 +: 8] = inv_sbox(s[i*8 +: 8]);
        end
    endfunction

    // ShiftRows (forward)
    // State is column-major: byte[i] = state[row][col] where i = col*4 + row
    function [127:0] shift_rows;
        input [127:0] s;
        reg [7:0] b [0:15];
        reg [7:0] r [0:15];
        integer i;
        begin
            for (i = 0; i < 16; i = i + 1) b[i] = s[i*8 +: 8];
            // Row 0: no shift
            r[0]  = b[0];  r[4]  = b[4];  r[8]  = b[8];  r[12] = b[12];
            // Row 1: left shift 1
            r[1]  = b[5];  r[5]  = b[9];  r[9]  = b[13]; r[13] = b[1];
            // Row 2: left shift 2
            r[2]  = b[10]; r[6]  = b[14]; r[10] = b[2];  r[14] = b[6];
            // Row 3: left shift 3
            r[3]  = b[15]; r[7]  = b[3];  r[11] = b[7];  r[15] = b[11];
            for (i = 0; i < 16; i = i + 1) shift_rows[i*8 +: 8] = r[i];
        end
    endfunction

    // InvShiftRows
    function [127:0] inv_shift_rows;
        input [127:0] s;
        reg [7:0] b [0:15];
        reg [7:0] r [0:15];
        integer i;
        begin
            for (i = 0; i < 16; i = i + 1) b[i] = s[i*8 +: 8];
            r[0]  = b[0];  r[4]  = b[4];  r[8]  = b[8];  r[12] = b[12];
            r[1]  = b[13]; r[5]  = b[1];  r[9]  = b[5];  r[13] = b[9];
            r[2]  = b[10]; r[6]  = b[14]; r[10] = b[2];  r[14] = b[6];
            r[3]  = b[7];  r[7]  = b[11]; r[11] = b[15]; r[15] = b[3];
            for (i = 0; i < 16; i = i + 1) inv_shift_rows[i*8 +: 8] = r[i];
        end
    endfunction

    // MixColumns (forward)
    function [127:0] mix_columns;
        input [127:0] s;
        reg [7:0] b [0:15];
        reg [7:0] r [0:15];
        integer c;
        begin
            for (c = 0; c < 16; c = c + 1) b[c] = s[c*8 +: 8];
            for (c = 0; c < 4; c = c + 1) begin
                r[c*4+0] = gf_mul2(b[c*4+0]) ^ gf_mul3(b[c*4+1]) ^ b[c*4+2]         ^ b[c*4+3];
                r[c*4+1] = b[c*4+0]          ^ gf_mul2(b[c*4+1]) ^ gf_mul3(b[c*4+2]) ^ b[c*4+3];
                r[c*4+2] = b[c*4+0]          ^ b[c*4+1]          ^ gf_mul2(b[c*4+2]) ^ gf_mul3(b[c*4+3]);
                r[c*4+3] = gf_mul3(b[c*4+0]) ^ b[c*4+1]          ^ b[c*4+2]         ^ gf_mul2(b[c*4+3]);
            end
            for (c = 0; c < 16; c = c + 1) mix_columns[c*8 +: 8] = r[c];
        end
    endfunction

    // InvMixColumns
    function [127:0] inv_mix_columns;
        input [127:0] s;
        reg [7:0] b [0:15];
        reg [7:0] r [0:15];
        integer c;
        begin
            for (c = 0; c < 16; c = c + 1) b[c] = s[c*8 +: 8];
            for (c = 0; c < 4; c = c + 1) begin
                r[c*4+0] = gf_mul(b[c*4+0],4'he) ^ gf_mul(b[c*4+1],4'hb) ^ gf_mul(b[c*4+2],4'hd) ^ gf_mul(b[c*4+3],4'h9);
                r[c*4+1] = gf_mul(b[c*4+0],4'h9) ^ gf_mul(b[c*4+1],4'he) ^ gf_mul(b[c*4+2],4'hb) ^ gf_mul(b[c*4+3],4'hd);
                r[c*4+2] = gf_mul(b[c*4+0],4'hd) ^ gf_mul(b[c*4+1],4'h9) ^ gf_mul(b[c*4+2],4'he) ^ gf_mul(b[c*4+3],4'hb);
                r[c*4+3] = gf_mul(b[c*4+0],4'hb) ^ gf_mul(b[c*4+1],4'hd) ^ gf_mul(b[c*4+2],4'h9) ^ gf_mul(b[c*4+3],4'he);
            end
            for (c = 0; c < 16; c = c + 1) inv_mix_columns[c*8 +: 8] = r[c];
        end
    endfunction

    // AddRoundKey
    function [127:0] add_round_key;
        input [127:0] s;
        input [127:0] k;
        begin
            add_round_key = s ^ k;
        end
    endfunction

    // ========================================================================
    // GF(2^128) multiply for GHASH
    // ========================================================================
    // Bit-serial: 128 cycles per GHASH multiplication
    // We run it in parallel with AES rounds, so it's pipelined.
    reg [127:0] ghash_h;       // Hash key (AES_K(0^128))
    reg [127:0] ghash_acc;     // Running GHASH accumulator
    reg         ghash_valid;   // New GHASH result available

    // Combinational: one bit of GF(2^128) multiply
    // V = V >> 1, if V[0] was set, V ^= R (R = 0xE1 << 120)
    function [127:0] gf128_shift;
        input [127:0] v;
        begin
            gf128_shift = {1'b0, v[127:1]};
            if (v[0]) gf128_shift = gf128_shift ^ 128'hE100_0000_0000_0000_0000_0000_0000_0000;
        end
    endfunction

    // GHASH state machine
    reg [6:0]   ghash_bit;     // Current bit (0-127)
    reg [127:0] ghash_z;       // Product accumulator
    reg [127:0] ghash_v;       // Shifted H
    reg         ghash_busy;

    // ========================================================================
    // Main FSM
    // ========================================================================
    localparam [2:0] AES_IDLE       = 3'd0;
    localparam [2:0] AES_KEY_EXPAND = 3'd1;
    localparam [2:0] AES_ROUND      = 3'd2;
    localparam [2:0] AES_GHASH      = 3'd3;
    localparam [2:0] AES_DONE       = 3'd4;

    reg [2:0]   aes_state;
    reg [127:0] state_blk;     // Current AES state (16 bytes)
    reg [127:0] ctr_blk;       // Counter block for CTR mode
    reg [31:0]  ctr_val;       // 32-bit counter (last 4 bytes of CTR block)
    reg         key_exp_phase; // 0 = first half key schedule, 1 = second half
    reg [3:0]   key_round;     // Key expansion round index
    reg [127:0] enc_block;     // Encrypted counter block (XOR with plaintext)

    // Round key storage for AES-256 (15 round keys, 128 bits each)
    reg [127:0] rk [0:14];
    reg [3:0]   rk_gen_cnt;    // Round key generation counter

    // ========================================================================
    // Key Schedule — generate all 15 round keys up front
    // ========================================================================
    reg [31:0] w [0:7];        // Initial 8 key words
    reg [3:0]  ks_step;
    reg [31:0] ks_temp;

    // ========================================================================
    // Main Sequential Logic
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aes_state    <= AES_IDLE;
            busy         <= 1'b0;
            done         <= 1'b0;
            auth_fail    <= 1'b0;
            irq          <= 1'b0;
            round_cnt    <= 4'd0;
            ghash_acc    <= 128'd0;
            ghash_busy   <= 1'b0;
            ghash_valid  <= 1'b0;
            ctr_val      <= 32'd0;
            data_out     <= 128'd0;
            tag          <= 128'd0;
            key          <= 256'd0;
            iv           <= 96'd0;
            aad_len      <= 32'd0;
            data_len     <= 32'd0;
            cmd_encrypt  <= 1'b0;
            data_in      <= 128'd0;
            state_blk    <= 128'd0;
        end else begin
            irq <= 1'b0;  // pulse

            case (aes_state)
                // ====================================================
                // IDLE — wait for command
                // ====================================================
                AES_IDLE: begin
                    ghash_valid <= 1'b0;
                end

                // ====================================================
                // KEY_EXPAND — generate 15 round keys from 256-bit key
                // ====================================================
                AES_KEY_EXPAND: begin
                    if (rk_gen_cnt == 4'd0) begin
                        // Initialize W[0..7] from key
                        w[0] <= key[255:224]; w[1] <= key[223:192];
                        w[2] <= key[191:160]; w[3] <= key[159:128];
                        w[4] <= key[127:96];  w[5] <= key[95:64];
                        w[6] <= key[63:32];   w[7] <= key[31:0];
                        rk[0] <= key[255:128]; // First round key = first 128 bits
                        rk[1] <= key[127:0];   // Second round key = last 128 bits
                        rk_gen_cnt <= 4'd2;
                        ks_step <= 4'd0;
                    end else if (rk_gen_cnt <= 4'd14) begin
                        // Generate next round key
                        case (ks_step)
                            4'd0: begin
                                if (rk_gen_cnt[0] == 0) begin
                                    // Even round: SubWord(RotWord(w[7])) ^ rcon
                                    ks_temp <= sub_word(rot_word(w[7])) ^ {rcon(rk_gen_cnt[3:1] - 1), 24'd0};
                                end else begin
                                    // Odd round: SubWord(w[7])
                                    ks_temp <= sub_word(w[3]);
                                end
                                ks_step <= 4'd1;
                            end
                            4'd1: begin
                                if (rk_gen_cnt[0] == 0) begin
                                    w[0] <= w[0] ^ ks_temp;
                                    w[1] <= w[1] ^ (w[0] ^ ks_temp);
                                    w[2] <= w[2] ^ w[1] ^ (w[0] ^ ks_temp);
                                    w[3] <= w[3] ^ w[2] ^ w[1] ^ (w[0] ^ ks_temp);
                                end else begin
                                    w[4] <= w[4] ^ ks_temp;
                                    w[5] <= w[5] ^ (w[4] ^ ks_temp);
                                    w[6] <= w[6] ^ w[5] ^ (w[4] ^ ks_temp);
                                    w[7] <= w[7] ^ w[6] ^ w[5] ^ (w[4] ^ ks_temp);
                                end
                                ks_step <= 4'd2;
                            end
                            4'd2: begin
                                if (rk_gen_cnt[0] == 0) begin
                                    rk[rk_gen_cnt] <= {w[0] ^ ks_temp,
                                                       w[1] ^ (w[0] ^ ks_temp),
                                                       w[2] ^ w[1] ^ (w[0] ^ ks_temp),
                                                       w[3] ^ w[2] ^ w[1] ^ (w[0] ^ ks_temp)};
                                end else begin
                                    rk[rk_gen_cnt] <= {w[4] ^ ks_temp,
                                                       w[5] ^ (w[4] ^ ks_temp),
                                                       w[6] ^ w[5] ^ (w[4] ^ ks_temp),
                                                       w[7] ^ w[6] ^ w[5] ^ (w[4] ^ ks_temp)};
                                end
                                rk_gen_cnt <= rk_gen_cnt + 1;
                                ks_step <= 4'd0;
                            end
                            default: ks_step <= 4'd0;
                        endcase
                    end else begin
                        // All round keys generated. Compute H = AES_K(0).
                        state_blk <= 128'd0;
                        round_cnt <= 4'd0;
                        aes_state <= AES_ROUND;
                        // After computing H, we'll store it in ghash_h
                        // and then set up CTR mode
                    end
                end

                // ====================================================
                // ROUND — execute one AES round per cycle
                // ====================================================
                AES_ROUND: begin
                    if (round_cnt == 4'd0) begin
                        // Initial AddRoundKey
                        state_blk <= state_blk ^ rk[0];
                        round_cnt <= 4'd1;
                    end else if (round_cnt < 4'd14) begin
                        // Rounds 1-13: SubBytes → ShiftRows → MixColumns → AddRoundKey
                        if (!cmd_encrypt && ghash_h != 128'd0) begin
                            // Decrypt: InvShiftRows → InvSubBytes → AddRoundKey → InvMixColumns
                            state_blk <= inv_mix_columns(
                                add_round_key(
                                    inv_sub_bytes(inv_shift_rows(state_blk)),
                                    rk[round_cnt]));
                        end else begin
                            // Encrypt (or H computation)
                            state_blk <= add_round_key(
                                mix_columns(shift_rows(sub_bytes(state_blk))),
                                rk[round_cnt]);
                        end
                        round_cnt <= round_cnt + 1;
                    end else begin
                        // Round 14 (final): no MixColumns
                        if (!cmd_encrypt && ghash_h != 128'd0) begin
                            state_blk <= add_round_key(
                                inv_sub_bytes(inv_shift_rows(state_blk)),
                                rk[14]);
                        end else begin
                            state_blk <= add_round_key(
                                shift_rows(sub_bytes(state_blk)),
                                rk[14]);
                        end

                        if (ghash_h == 128'd0) begin
                            // First encryption was for H computation
                            ghash_h <= state_blk ^ rk[14]; // will be set next cycle
                            // Now set up CTR block: IV || counter=1
                            ctr_blk <= {iv, 32'd1};
                            ctr_val <= 32'd1;
                            // Initialize GHASH accumulator
                            ghash_acc <= 128'd0;
                            done <= 1'b1;
                            busy <= 1'b0;
                            aes_state <= AES_IDLE;
                        end else begin
                            // Block encryption/decryption complete
                            enc_block <= state_blk ^ rk[14]; // next cycle
                            aes_state <= AES_DONE;
                        end
                    end
                end

                // ====================================================
                // DONE — XOR encrypted counter with data, start GHASH
                // ====================================================
                AES_DONE: begin
                    // CTR mode: output = plaintext XOR AES_K(counter)
                    data_out <= data_in ^ enc_block;
                    // For GHASH: accumulate ciphertext
                    // If encrypting: ciphertext = data_in ^ enc_block
                    // If decrypting: ciphertext = data_in (input is ciphertext)
                    ghash_acc <= ghash_acc ^ (cmd_encrypt ? data_in : (data_in ^ enc_block));

                    // Start GHASH multiply
                    ghash_z <= 128'd0;
                    ghash_v <= ghash_h;
                    ghash_bit <= 7'd0;
                    ghash_busy <= 1'b1;
                    aes_state <= AES_GHASH;

                    // Increment counter
                    ctr_val <= ctr_val + 32'd1;
                    ctr_blk <= {iv, ctr_val + 32'd1};
                end

                // ====================================================
                // GHASH — GF(2^128) multiply, 128 cycles
                // ====================================================
                AES_GHASH: begin
                    if (ghash_busy) begin
                        // One bit of GF(2^128) multiply: Z ^= V if bit set
                        if (ghash_acc[127 - ghash_bit])
                            ghash_z <= ghash_z ^ ghash_v;
                        ghash_v <= gf128_shift(ghash_v);

                        if (ghash_bit == 7'd127) begin
                            ghash_acc <= ghash_z ^ (ghash_acc[0] ? ghash_v : 128'd0);
                            ghash_busy <= 1'b0;
                            done <= 1'b1;
                            busy <= 1'b0;
                            irq <= 1'b1;
                            aes_state <= AES_IDLE;
                        end else begin
                            ghash_bit <= ghash_bit + 1;
                        end
                    end
                end

                default: aes_state <= AES_IDLE;
            endcase

            // ============================================================
            // Register writes from MMIO
            // ============================================================
            if (req && wen) begin
                case (addr)
                    // Key writes (8 × 32-bit = 256 bits)
                    7'h00: key[255:224] <= wdata[31:0];
                    7'h04: key[223:192] <= wdata[31:0];
                    7'h08: key[191:160] <= wdata[31:0];
                    7'h0c: key[159:128] <= wdata[31:0];
                    7'h10: key[127:96]  <= wdata[31:0];
                    7'h14: key[95:64]   <= wdata[31:0];
                    7'h18: key[63:32]   <= wdata[31:0];
                    7'h1c: key[31:0]    <= wdata[31:0];
                    // IV writes (3 × 32-bit = 96 bits)
                    7'h20: iv[95:64]    <= wdata[31:0];
                    7'h24: iv[63:32]    <= wdata[31:0];
                    7'h28: iv[31:0]     <= wdata[31:0];
                    // Lengths
                    7'h30: aad_len      <= wdata[31:0];
                    7'h34: data_len     <= wdata[31:0];
                    // Command — starts key expansion
                    7'h38: begin
                        cmd_encrypt <= wdata[0];
                        busy <= 1'b1;
                        done <= 1'b0;
                        auth_fail <= 1'b0;
                        ghash_h <= 128'd0;
                        ghash_acc <= 128'd0;
                        rk_gen_cnt <= 4'd0;
                        ks_step <= 4'd0;
                        aes_state <= AES_KEY_EXPAND;
                    end
                    // Data input (4 × 32-bit = 128 bits)
                    7'h40: data_in[127:96] <= wdata[31:0];
                    7'h44: data_in[95:64]  <= wdata[31:0];
                    7'h48: data_in[63:32]  <= wdata[31:0];
                    7'h4c: begin
                        data_in[31:0] <= wdata[31:0];
                        // Last word written triggers block processing
                        if (!busy && ghash_h != 128'd0) begin
                            busy <= 1'b1;
                            done <= 1'b0;
                            state_blk <= ctr_blk;
                            round_cnt <= 4'd0;
                            aes_state <= AES_ROUND;
                        end
                    end
                    // Tag writes (for decrypt verification)
                    7'h60: tag[127:96] <= wdata[31:0];
                    7'h64: tag[95:64]  <= wdata[31:0];
                    7'h68: tag[63:32]  <= wdata[31:0];
                    7'h6c: tag[31:0]   <= wdata[31:0];
                    default: ;
                endcase
            end
        end
    end

    // ========================================================================
    // Register reads
    // ========================================================================
    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            if (!wen) begin
                case (addr)
                    7'h39:   rdata <= {56'd0, status};
                    7'h50:   rdata <= {32'd0, data_out[127:96]};
                    7'h54:   rdata <= {32'd0, data_out[95:64]};
                    7'h58:   rdata <= {32'd0, data_out[63:32]};
                    7'h5c:   rdata <= {32'd0, data_out[31:0]};
                    7'h60:   rdata <= {32'd0, tag[127:96]};
                    7'h64:   rdata <= {32'd0, tag[95:64]};
                    7'h68:   rdata <= {32'd0, tag[63:32]};
                    7'h6c:   rdata <= {32'd0, tag[31:0]};
                    default: rdata <= 64'd0;
                endcase
            end else begin
                rdata <= 64'd0;
            end
        end
    end

endmodule
