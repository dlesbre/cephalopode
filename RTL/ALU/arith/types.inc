// Cephalopode-wide types (todo: include a single cephalopode-wide file located higher in the tree)
type int => "int";
type addr => "addr";
type node => "node";
type simple_Int => "simple_Int";
type Int => "Int";
fields Int = [simple_Int:simple_Int, msb:bit, lsb:bit];
type g_type => "g_type";
fields node = [laddr:addr, raddr:addr, "type":g_type, int:Int];

// And constants
define G_INT = ("G_INT" :: g_type);
define G_NIL = ("G_NIL" :: g_type);
define G_APPLY = ("G_APPL" :: g_type);
define ADDR_END = ("addr_end" :: addr);

// ALU-only types
type op => "op";
fields op = [dest:addr, srcA:addr, srcB:addr, srcC:addr];
type simple_dInt => "simple_dInt";
type dInt => "dInt";
fields dInt = [simple:simple_dInt, high:Int, low:Int];
type eInt => "eInt";
