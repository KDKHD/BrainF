//Brainf*** Compiler

object BFComp {


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}

def load_bff(name: String) : String = {
      try {
            io.Source.fromFile(name).getLines.toList.mkString(" ")
      } 
      catch {
           case e: Exception => ""
      }
}

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
      if(pc >= prog.length){
            prog.length
      }
      else if(prog.charAt(pc) == "]".charAt(0) && level == 0){
            //found end
            pc + 1
      }
      else if(prog.charAt(pc) == "[".charAt(0)){
           jumpRight(prog, pc+1, level+1) 
      }
      else if(prog.charAt(pc) == "]".charAt(0)){
           jumpRight(prog, pc+1, level-1) 
      }
      else{
           jumpRight(prog, pc+1, level) 
      }
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
      if(pc < 0){
            -1
      }
      else if(prog.charAt(pc) == "[".charAt(0) && level == 0){
            //found end
            pc + 1
      }
      else if(prog.charAt(pc) == "[".charAt(0)){
           jumpLeft(prog, pc-1, level-1) 
      }
      else if(prog.charAt(pc) == "]".charAt(0)){
           jumpLeft(prog, pc-1, level+1) 
      }
      else{
           jumpLeft(prog, pc-1, level) 
      }
}

def jtable(pg: String) : Map[Int, Int] = {
    val toFilter = for(x <-(0 until pg.length)) yield( 
        pg.charAt(x).toString match {
          case "[" => (x -> jumpRight(pg, x + 1, 0))
          case "]" => (x -> jumpLeft(pg, x - 1, 0))
          case _ => (-2 -> -2)
        }
    )
    toFilter.toMap.-(-2)
  }


def sread(mem: Mem, mp: Int) : Int = {
      if(mem isDefinedAt mp){
            mem(mp)
      }
      else{
            0
      }
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
      mem.updated(mp, v)
}

//def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ...
def compute2(prog: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
      if(pc >= prog.length || pc < 0){
            mem
      }
      else{
            prog.charAt(pc).toString match {
			 case ">" => compute2(prog, tb, pc+1, mp+1, mem)
			 case "<" => compute2(prog, tb, pc+1, mp-1, mem)
			 case "+" => compute2(prog, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
			 case "-" => compute2(prog, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
                   case "." => {
                         print(mem(mp).asInstanceOf[Char])
                         compute2(prog, tb, pc+1, mp, mem)
                         }
                   case "," => {
                         val input = Console.in.read().toByte
                         compute2(prog, tb, pc+1, mp, mem.updated(mp, input))
                   }
                   case "[" => {
                         if (sread(mem, mp)==0) compute2(prog, tb, tb(pc), mp, mem) else compute2(prog, tb,  pc+1, mp, mem)
                   }
                   case "]" => {
                         if(sread(mem, mp)!=0) compute2(prog, tb, tb(pc), mp, mem) else compute2(prog, tb,  pc+1, mp, mem)
                   }
                   case _ => compute2(prog, tb,  pc+1, mp, mem)
		 }
      }
}


def run2(pg: String, m: Mem = Map()) = {
        compute2(pg, jtable(pg) ,0,0,m)
}


// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("seirpinski.bf")))

def optimise(s: String) : String = {
    s.replaceAll("""[^<>+-.,\[\]]""", "").replaceAll("""\[-\]""", "0")
}

def compute3(prog: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if(pc >= prog.length || pc < 0){
            mem
      }
      else{
            prog.charAt(pc).toString match {
        case "0" => {
          compute3(prog, tb,  pc+1, mp, write(mem, mp, 0))
        }
			 case ">" => compute3(prog, tb, pc+1, mp+1, mem)
			 case "<" => compute3(prog, tb, pc+1, mp-1, mem)
			 case "+" => compute3(prog, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
			 case "-" => compute3(prog, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
                   case "." => {
                         print(mem(mp).asInstanceOf[Char])
                         compute3(prog, tb, pc+1, mp, mem)
                         }
                   case "," => {
                         val input = Console.in.read().toByte
                         compute3(prog, tb, pc+1, mp, mem.updated(mp, input))
                   }
                   case "[" => {
                         if (sread(mem, mp)==0) compute3(prog, tb, tb(pc), mp, mem) else compute3(prog, tb,  pc+1, mp, mem)
                   }
                   case "]" => {
                         if(sread(mem, mp)!=0) compute3(prog, tb, tb(pc), mp, mem) else compute3(prog, tb,  pc+1, mp, mem)
                   }
                   case _ => compute3(prog, tb,  pc+1, mp, mem)
		 }
      }
}

def run3(pg: String, m: Mem = Map()) = {
  val optimiseProg = optimise(pg)
  compute3(optimiseProg, jtable(optimiseProg) ,0,0,m)
}


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11203
// 
// time_needed(1, run3(load_bff("benchmark.bf")))


val alphabet = List("A", "B", "C", "D", "E", "F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")


def replacePlus(s: String, c: Int = 26) : String = {
  if(c == 0){
    s
  }
  else{
    replacePlus(s.replaceAll("""(\+){"""+c+"""}(?![a-zA-Z])""","+"+alphabet(c-1)), c-1)
  }
}

def replaceMinus(s: String, c: Int = 26) : String = {
  if(c == 0){
    s
  }
  else{
    replaceMinus(s.replaceAll("""(\-){"""+c+"""}(?![a-zA-Z])""","-"+alphabet(c-1)), c-1)
  }
}

def replaceLeft(s: String, c: Int = 26) : String = {
  if(c == 0){
    s
  }
  else{
    replaceLeft(s.replaceAll("""(\<){"""+c+"""}(?![a-zA-Z])""","<"+alphabet(c-1)), c-1)
  }
}

def replaceRight(s: String, c: Int = 26) : String = {
  if(c == 0){
    s
  }
  else{
    replaceRight(s.replaceAll("""(\>){"""+c+"""}(?![a-zA-Z])""",">"+alphabet(c-1)), c-1)
  }
}


def combine(s: String) : String = {
  replaceRight(replaceLeft(replaceMinus(replacePlus(s))))
}

// testcase
// combine(load_bff("benchmark.bf"))

def compute4(prog: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
      if(pc >= prog.length || pc < 0){
            mem
      }
      else{
            prog.charAt(pc) match {
              case '0' => compute4(prog, tb,  pc+1, mp, write(mem, mp, 0))
              case '>' => compute4(prog, tb, pc+2, mp+(prog.charAt(pc+1).toInt - 64), mem)
              case '<' => compute4(prog, tb, pc+2, mp-(prog.charAt(pc+1).toInt - 64), mem)
              case '+' => compute4(prog, tb, pc+2, mp, write(mem, mp, sread(mem, mp)+(prog.charAt(pc+1).toInt - 64)))
              case '-' => compute4(prog, tb, pc+2, mp, write(mem, mp, sread(mem, mp)-(prog.charAt(pc+1).toInt - 64)))
              case '.' => {
                print(mem(mp).asInstanceOf[Char])
                compute4(prog, tb, pc+1, mp, mem)
              }
              case ',' => {
                val input = Console.in.read().toByte
                compute4(prog, tb, pc+1, mp, mem.updated(mp, input))
              }
              case '[' => {
                if (sread(mem, mp)==0) compute4(prog, tb, tb(pc), mp, mem) else compute4(prog, tb,  pc+1, mp, mem)
              }
              case ']' => {
                if(sread(mem, mp)!=0) compute4(prog, tb, tb(pc), mp, mem) else compute4(prog, tb,  pc+1, mp, mem)
              }
              case _ => compute4(prog, tb,  pc+1, mp, mem)
		 }
      }
}


// should call first optimise and then combine on the input string
def run4(pg: String, m: Mem = Map()) = {
  val op = combine(optimise(pg))
  compute4(op, jtable(op), 0, 0, m)
}


// testcases
// .combine(BFComp.optimise(BFComp.load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))
//BFComp.time_needed(1, BFComp.run4(BFComp.load_bff("benchmark.bf")))
//BFComp.time_needed(1, BFComp.run4(BFComp.load_bff("sierpinski.bf")))

}
 

