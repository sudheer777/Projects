package sai;

import java.io.* ;

public class OcfLZW {
    /*
     * 	** This is the decompression routine.
     * 		*/
    final static int INIT_BITS = 9;
    final int MAX_BITS = 13;
    final int TABLE_SIZE = 9029;
    final int HASH_SHIFT = 5;
    final int CLEAR_TABLE = 256;
    final int TERMINATOR = 257;
    final static int FIRST_CODE = 258;
    final static int CHECK_TIME = 100;
    final int STACK_SIZE = 4000;
    boolean EOF=false;

    static int myIndex,
            offset,
            input_bit_buffer,
            output_bit_buffer,
            string_code,                    /* contains string codes */
            bytes_out_this_time,             /* used to count compressed rec size */
            next_code = FIRST_CODE,          /* next code to be assigned */
            max_code,
            checkpoint = CHECK_TIME,         /* for compression ratio monitoring */
            tot_bytesin = 0,
            gbl_ulong;

    static int[] code_value = null;
    static int[] prefix_code = null;

    int input_bit_count=0,
            output_bit_count=0,
            num_bits = INIT_BITS,
            clear_bytesin = 0,
            clear_bytesout = 0,
            tot_bytesout = 0,
            ratio_new,                           /* new compression ratio as a % */
            ratio_old = 100;                     /* orig. ratio at 100%*/

    static boolean compress_flag = false;
    static int initialized = 0;

    static byte[] in_buffpoint = null;
    static byte[] out_buffpoint = null;
    int in_buff_n=0,out_buff_n=0,in_buffend=0, out_buffend=0;

    byte[] append_character = null;
    byte gbl_ptr_idx = 0;

    byte[] decode_stack = null;        /* For expand only */
    int  ds_i =  0;

    // reset static data
    public OcfLZW() {
        myIndex = 0;
        offset = 0;
        input_bit_buffer = 0;
        output_bit_buffer = 0;
        string_code = 0;                    /* contains string codes */
        bytes_out_this_time = 0;             /* used to count compressed rec size */
        next_code = FIRST_CODE ;          /* next code to be assigned */
        max_code = 0;
        checkpoint = CHECK_TIME ;        /* for compression ratio monitoring */
        tot_bytesin = 0 ;
        gbl_ulong = 0;
        code_value = null;
        prefix_code = null;
        input_bit_count=0  ;
        output_bit_count=0 ;
        num_bits = INIT_BITS  ;
        clear_bytesin = 0 ;
        clear_bytesout = 0;
        tot_bytesout = 0;
        ratio_new = 0;                           /* new compression ratio as a % */
        ratio_old = 100;                     /* orig. ratio at 100%*/

        compress_flag = false;
        initialized = 0;

        in_buffpoint = null;
        out_buffpoint = null;
        in_buff_n=0;out_buff_n=0;in_buffend=0; out_buffend=0;

        append_character = null;
        gbl_ptr_idx = 0;

        decode_stack = null;        /* For expand only */
        ds_i =  0;


    }

    public int	expand(byte [] input_string, byte[] output_string)
    {
        ds_i = 0;

        compress_flag = false;

        /* Initialization code for both compress and expand. */
        max_code = (( 1<<( num_bits )) -1);
        prefix_code = new int[ TABLE_SIZE];
        append_character = new byte[TABLE_SIZE];

        /* allocate the decode stack dynamically */

        decode_stack = new byte[STACK_SIZE];

        if (prefix_code == null        ||
                append_character == null   ||
                decode_stack == null       ||
                (compress_flag && code_value == null ))
            return 0;

        out_buffpoint = output_string;
        out_buff_n = 0;
        in_buffpoint = input_string;
        in_buff_n = 0;


        /******  LOCAL VARIABLES  ******/
        int new_code[] = new int[1];
        int old_code = 0;
        int character=0;
        short clear_flag;
        int stack_top[]=new int[1];
        int out_size = 0;
        new_code[0]=0;

        stack_top[0]=0;
        /***********  BODY  ***********/
        clear_flag = 1;

        /*
         * 	    ** This is the main expansion loop.  Read in characters from the compressed
         * 	    	    ** buffer until input_code returns TERMINATOR meaning we are at end of buffer.
         * 	    	    	    */
        inputCode(new_code);
        while (!EOF && new_code[0]!= TERMINATOR) {
            if (clear_flag >  0) {
                old_code = new_code[0];
                character = old_code;

                if (new_code[0] <= 255) {
                    clear_flag = 0;
                    if (putByte(old_code)==0)
                        return 0;
                    out_size++;
                    inputCode(new_code);
                    continue;
                }
            }

            if (new_code[0] == CLEAR_TABLE) {
                clear_flag = 1;
                num_bits = INIT_BITS;
                next_code = FIRST_CODE;
                max_code = MAXVAL(num_bits);
                inputCode(new_code);
                continue;
            }
            /*
             * 	        ** This code checks for the special STRING+CHARACTER+STRING+CHAR+STRING
             * 	        	        ** case which generates an undefined code.  It handles it by decoding
             * 	        	        	        ** the last code, adding a single char to the end of the decode string.
             * 	        	        	        	        */
            if ( new_code[0] >= next_code) {
                decode_stack[ds_i] =  (byte)character;
                if (decodeString(stack_top,decode_stack,ds_i+1,old_code)==0)
                    return 0;

            } else {   /* do a straight decode of the new_code */
                if (decodeString(stack_top,decode_stack,ds_i,new_code[0])==0)
                    return 0;
            }

            /*
             * 	        ** Output the decoded string in reverse order.
             * 	        	        */
            character = decode_stack[stack_top[0]];

            while (stack_top[0] >= ds_i) {
                if (putByte(decode_stack[stack_top[0]--])==0)
                    return 0;
                out_size++;
            }

            /*
             * 	        ** Finally, if possible, add a new code to the string table.
             * 	        	        */
            if (clear_flag==0) {
                if (next_code <= max_code) {
                    prefix_code[next_code] = old_code;
                    append_character[next_code++] =  (byte)character;
                    if (next_code == max_code && num_bits < MAX_BITS)
                        max_code = MAXVAL(++num_bits);
                }
            }
            else
                clear_flag = 0;
            old_code = new_code[0];
            inputCode(new_code);
        }
        return out_size;
    }
    void inputCode(  int[] rtn )
    {
        while (input_bit_count <= 24) {
            if (in_buff_n== in_buffpoint.length) {
                EOF=true;
                break;
            } else {
                tot_bytesin++;
                rtn[0] = (in_buffpoint[in_buff_n++] & 0xff);
            }
            input_bit_buffer |= (rtn[0] << (24 - input_bit_count));
            input_bit_count += 8;
        }

        if (!EOF) {
            rtn[0] = (input_bit_buffer >>> (32 - num_bits));
            input_bit_buffer <<= num_bits;
            input_bit_count -= num_bits;
        }

        return;
    }


    /*
     * 	** This routine writes the code to the output buffer.  If the output
     * 		** buffer is full, the routine returns an error (0).
     * 			** This routine was made a macro for performance reasons.
     * 				*/
    int
    putByte( int code )
    {
        if (tot_bytesout == out_buffpoint.length)
            return 0;

        out_buffpoint[tot_bytesout] = (byte)code;
        tot_bytesout++;
        return 1;
    }
    int MAXVAL (int n)
    {
        return (( 1<<( n )) -1);
    }

    /*
     * ** Decode a string from the string table, store it in a buffer.
     * ** The buffer can then be output in reverse order.  A 0 return signals
     * ** an error. This routine has been made a macro for performance reasons.
     * */
    int decodeString(  int[] stack_top, byte[] ptr,int ds_i, int code )
    {
        gbl_ulong = code;
        myIndex = 0;
        while (gbl_ulong > 255) {
            if (gbl_ulong >= TABLE_SIZE)
                return 0;


            ptr[ds_i++] = append_character[gbl_ulong];
            gbl_ulong = prefix_code[gbl_ulong];

            if (myIndex++ >= STACK_SIZE)
                return 0;
        }
        ptr[ds_i] = (byte)gbl_ulong;
        stack_top[0] = ds_i;
        return 1;
    }

    public static byte[] hexStringToByteArray(String s) {
        int len = s.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4)
                    + Character.digit(s.charAt(i+1), 16));
        }
        return data;
    }

}
