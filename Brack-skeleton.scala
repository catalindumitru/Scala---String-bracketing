/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 100

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}

	def IntToLetter(n: Int): Char = {
		return (n+65).toChar
	}

  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)
  op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] =
    scala.io.Source.fromFile(fname).toArray.init


  /** Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z

	def PossibleRec(w: Array[Int], i: Int, j: Int, z: Int): Boolean = {
		require (0 <= i && i < w.size && 0 <= j && j <= w.size)
		if (j <= i) return false
		if (j-i == 1) return w(i) == z
		for (u <- 0 to 2; v <- 0 to 2) // consider u, v s.t. uv = z
				if(op(u)(v) == z)
					for (k <- i+1 until j){
						// if we can paranthesise w[i..k) into u and w[k..j) into v then
						// we can paranthesise w[i..j) into uv = z
						val check = PossibleRec(w,i,k,u) && PossibleRec(w,k,j,v)
						if (check) return true
					}
		return false
	}


	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z

	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		require (0 <= i && i < w.size && 0 <= j && j <= w.size)
		if (j <= i) return 0
		if (j-i == 1) { if (w(i) == z) return 1 else return 0 }
		var total = 0
		for (u <- 0 to 2; v <- 0 to 2)
				if(op(u)(v) == z) // only executed 3 times; see table of the op
					for (k <- i+1 until j){
						val left = NumberRec(w,i,k,u)
						val right = NumberRec(w,k,j,v)
						total += left*right
					}
		return total
	}


	// TASK 3
	// Runtime analysis of recursive solution along with tests
	// Let T(j-i) be the time it takes NumberRec(w,i,j,z) to return a value.
	// We want to find T(n) as the initial call is NumberRec(w,0,w.size,z). Then
	// T(n) = 3*(T(1)+T(n-1)+T(2)+T(n-2)+...+T(n-1)+T(1)) + O(1)
	//      = 6*(T(1)+T(2)+...+T(n-1)) + O(1)
	//      = 6*(T(n-1)/6 + T(n-1)) + O(1)
	//      = 7*T(n-1) + O(1) = ... = O(7^n)
	// by direct recursion. A change of variable and the Master Theorem is another
	// aproach that works and gives O(7^n)

	/**
	tr02[~/Practicals/DAA]$ time scala Brack -PossibleRec
	BBBBBBBBBBBB
	Bracketing values for BBBBBBBBBBBB
	A is not Possible
	B is Possible
	C is not Possible

	real	0m5.674s
	user	0m4.636s
	sys	0m0.119s
	-----------------------------------------------------
	tr02[~/Practicals/DAA]$ time scala Brack -PossibleRec
	BBBBBBBBBBBBB
	Bracketing values for BBBBBBBBBBBBB
	A is not Possible
	B is Possible
	C is not Possible

	real	0m20.464s
	user	0m17.433s
	sys	0m0.179s
	-----------------------------------------------------
	tr02[~/Practicals/DAA]$ time scala Brack -PossibleRec
	BBBBBBBBBBBBBB
	Bracketing values for BBBBBBBBBBBBBB
	A is not Possible
	B is Possible
	C is not Possible

	real	1m23.288s
	user	1m19.909s
	sys	0m0.415s
	---------------------------------------------------

	tr02[~/Practicals/DAA]$ time scala Brack -NumberRec
	BBBBBBBBBB
	Bracketing values for BBBBBBBBBB
	A can be achieved in 0 ways
	B can be achieved in 4862 ways
	C can be achieved in 0 ways

	real	0m5.508s
	user	0m4.146s
	sys	0m0.114s
	---------------------------------------------------
	tr02[~/Practicals/DAA]$ time scala Brack -NumberRec
	BBBBBBBBBBB
	Bracketing values for BBBBBBBBBBB
	A can be achieved in 0 ways
	B can be achieved in 16796 ways
	C can be achieved in 0 ways

	real	0m24.422s
	user	0m21.385s
	sys	0m0.215s
	--------------------------------------------------
	tr02[~/Practicals/DAA]$ time scala Brack -NumberRec
	BBBBBBBBBBBB
	Bracketing values for BBBBBBBBBBBB
	A can be achieved in 0 ways
	B can be achieved in 58786 ways
	C can be achieved in 0 ways

	real	2m42.604s
	user	2m37.057s
	sys	0m0.677s

	*/

	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree): Unit = {
		t match {
			case t : Leaf => print(t.value)
			case t : Node => {
				val right = t.right
				val left = t.left
				left match {
					case left : Node => print("(")
					case _ => print("")
				}
				print_tree(t.left);

				left match {
					case left : Node => print(")")
					case _ => print("")
				}
				right match {
					case right : Node => print("(")
					case _ => print("")
				}
				print_tree(t.right)
				right match {
					case right : Node => print(")")
					case _ => print("")
				}
			}
		}
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution

	def Tabulate(w: Array[Int], n: Int): Unit = {
		// fill the diagonal above the main diagonal
		for (j <- 1 to n; z <- 0 to 2)
			poss(j-1)(j)(z) = w(j-1) == z
		// fill the remaining triangle from the upper matrix,
		// column by column from left to right, and each column from bottom to top
		for (j <- 2 to n; i <- j-2 to 0 by -1; z <- 0 to 2){
			poss(i)(j)(z) = false
			for (u <- 0 to 2; v <- 0 to 2)
				if (op(u)(v) == z)
					for (k <- i+1 to j-1)
						poss(i)(j)(z) = poss(i)(j)(z) || (poss(i)(k)(u) && poss(k)(j)(v))
		}

		//fill the diagonal above the main diagonal
		for (j <- 1 to n; z <- 0 to 2)
			if(w(j-1) == z)
				ways(j-1)(j)(z) = 1
			else ways(j-1)(j)(z) = 0
		// fill the remaining triangle from the upper matrix,
		// column by column from left to right, and each column from bottom to top
		for (j <- 2 to n; i <- j-2 to 0 by -1; z <- 0 to 2){
			ways(i)(j)(z) = 0
			for (u <- 0 to 2; v <- 0 to 2)
				if (op(u)(v) == z)
					for (k <- i+1 to j-1)
						ways(i)(j)(z) += ways(i)(k)(u) * ways(k)(j)(v)
		}

		for (j <- 1 to n; z <- 0 to 2)
			if(w(j-1) == z)
				exp(j-1)(j)(z) = Leaf(IntToLetter(z))
		for (j <- 2 to n; i <- j-2 to 0 by -1; z <- 0 to 2){
			for (u <- 0 to 2; v <- 0 to 2)
				if (op(u)(v) == z)
					for (k <- i+1 to j-1)
						if (poss(i)(k)(u) && poss(k)(j)(v))
							exp(i)(j)(z) = Node(exp(i)(k)(u), exp(k)(j)(v))
		}
	}

	// Task 6
	// Runtime analysis of dynamic programming version with tests

	// The maximum word length for which the program can correctly determine the
	// number of bracketings for all symbols is 20. This happens at the boundary
	// between test like BBBBBBBBBBBBBBBBBBBB and BBBBBBBBBBBBBBBBBBBBB after
	// which it may return correct values for A, say, but no values for B or c
	// because we go above Int.MaxValue.

	// The new dynamic approach should take time O(n^3) because of the 3 "for"
	// loops that fill the lists.

	// tw11[~/Practicals/DAA]$ time scala Brack -Tabulate
	// BBBBBBBBBB
	// Bracketing values for BBBBBBBBBB
	// A cannot be achieved
	// B can be achieved in 4862 ways
	// For example:((((((((BB)B)B)B)B)B)B)B)B
	// C cannot be achieved
	//
	// real	0m2.571s
	// user	0m0.645s
	// sys	0m0.047s
	// --------------------------------------------------

	// tw11[~/Practicals/DAA]$ time scala Brack -Tabulate
	// BBBBBBBBBBB
	// Bracketing values for BBBBBBBBBBB
	// A cannot be achieved
	// B can be achieved in 16796 ways
	// For example:(((((((((BB)B)B)B)B)B)B)B)B)B
	// C cannot be achieved
	//
	// real	0m10.283s
	// user	0m0.655s
	// sys	0m0.056s
	// --------------------------------------------------

	// tw11[~/Practicals/DAA]$ time scala Brack -Tabulate
	// BBBBBBBBBBBB
	// Bracketing values for BBBBBBBBBBBB
	// A cannot be achieved
	// B can be achieved in 58786 ways
	// For example:((((((((((BB)B)B)B)B)B)B)B)B)B)B
	// C cannot be achieved
	//
	// real	0m18.806s
	// user	0m0.666s
	// sys	0m0.068s
	// --------------------------------------------------

	// tw11[~/Practicals/DAA]$ time scala Brack -Tabulate
	// ABCABCABCABCABCABC
	// Bracketing values for ABCABCABCABCABCABC
	// A can be achieved in 58748411 ways
	// For example:((((((A(BC))A)(B(CA)))(B(CA)))(B(CA)))(B(CA)))(BC)
	// B can be achieved in 43862057 ways
	// For example:(((((A(BC))(A(BC)))(A(BC)))(A(BC)))(A(BC)))(A(BC))
	// C can be achieved in 27034322 ways
	// For example:(((((((A(BC))A)B)(C(AB)))(C(AB)))(C(AB)))(C(AB)))C
	//
	// real	0m4.141s
	// user	0m0.691s
	// sys	0m0.055s

	// The dynamic approach is an iterative one, compared to the recursive one.
	// This means that we are not in danger to overflow the recursion stack and
	// the memory used for the arrays is O(MAXWORD^2) which is not much when we
	// see how quickly the ways of bracketing go beyond Int.MaxValue even for
	// small MAXWORD.

	// Also, the time difference is enormous. Even for small words the recursive
	// version is infeasible, as a length of 11 takes a few minutes to run and
	// then the times gets 7 times larger with every increment in length. On the
	// other side, the dynamic programming approach takes 8 times more time only
	// when we double the size of the input, which is way more efficient.
	// (7^n >> n^3)


/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString =
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"

		if (args.length > 2){
			println(errString)
			sys.exit
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) =
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit;
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}

		//Executing appropriate command
    if(command == "-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			if(PossibleRec(plainInt, 0, len, i)){
				println(('A'.toInt + i).toChar + " is Possible");
			}
			else{
				println(('A'.toInt + i).toChar + " is not Possible");
			}
		}
    }
    else if(command == "-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			if(z == 1){
				printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
			}
			else{
				printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command == "-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved in %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved in %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }
    else println(errString)
  }
}
