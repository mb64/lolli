CM.make ();

fun export_ll () = 
  let fun aux (callname, args) = (Interpreter.ll (); OS.Process.success)
  in SMLofNJ.exportFn("../bin/lolli.heap",aux) end;

export_ll ();
