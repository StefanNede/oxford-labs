/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn
import java.lang.Math

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 30 

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}
	
  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)  
  op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray.init

 
  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z
	
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
		if ((j-i) == 1) {
			w(i) == z
		} else {
			var res = false
			// find possible (x,y) pair that give z
			for (x <- 0 until 3) {
				for (y <- 0 until 3) {
					if (op(x)(y) == z) {
						for (k <- i+1 until j) {
							// see if such a pairing is possible
							res = res || (PossibleRec(w, i, k, x) && PossibleRec(w, k, j, y))
						}
					}
				}
			}
			res
		}
	}

	
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		if (j-i == 1) {
			if (w(i) == z) {
				1
			} else {
				0
			}
		} else {
			var res = 0
			for (x <- 0 until 3) {
				for (y <- 0 until 3) {
					if (op(x)(y) == z) {
						for (k <- i+1 until j) {
							// add the number of ways such a combination can happen
							// number of ways to get x * number of ways to get y
							res += (NumberRec(w,i,k,x) * NumberRec(w,k,j,y))
						}
					}
				}
			}
			res 
		}
	} 

	
	//TASK 3
	/* Runtime analysis of recursive solution along with tests
	PossibleRec(w,0,n,z) should take O(7^n) in the worst case:
		in the worst case recursive call happens at every iteration -> T(n) = 6*sum(T(k)) as for go over 3 x's and 3 y's
		=> T(n) = 6(T(n-1)+T(n-2)+...+T(1)) where T(1) = 1
		=> T(n-1) = 6(T(n-2) + ... + T(1))
		=> T(n) = T(n-1) + 6*T(n-1) = 7*T(n-1) => T(n) = O(7^n)
	PossibleRec on ABBA (n = 4) takes 0.74s 
	PossibleRec on ABBAABBA (n = 8) takes 0.74s
	PossibleRec on ABBAABBAABBAABBA (n = 16) takes 0.77s
	PossibleRec on ABBA*6 (n = 24) takes 0.75s
	You can see that in practice the running time is similar even when n increases for the previous test cases 
	because the recursive call doesn't happen every time -> once a correct bracketing is found the compiler stops running it (once res is True)

	NumberRec(w,0,n,z) should take O(7^n) in the worst case as the recurrence is the same 
	NumberRec on ABBA (n = 4) takes 0.77s 
	NumberRec on ABBAABBA (n = 8) takes 0.92s
	NumberRec on ABBAABBAABBAABBA (n = 16) takes too long (got bored of waiting)
	You can now clearly see the exponential running time 
	*/
	
	
	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree) : Unit = t match {
		//TODO(optional)
    case Leaf(value) => print(value)
    case Node(left, right) => {
      print("(")
      print_tree(left)
      print_tree(right)
      print(")")
    }
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution
	
  var numToChar = Array('A','B','C')
	
	def Tabulate(w: Array[Int], n: Int): Unit = {
		// initialisation of table 
		// add single elements 
		for (i <- 0 until n) {
			poss(i)(i+1)(w(i)) = true
			ways(i)(i+1)(w(i)) = 1
      exp(i)(i+1)(w(i)) = Leaf(numToChar(w(i)))
		}

    // also add all combinations from the multiplication table for the expressions
    for (i <- 0 until n-1) {
        exp(i)(i+2)(op(w(i))(w(i+1))) = Node(Leaf(numToChar(w(i))), Leaf(numToChar(w(i+1))))
    }

		// fill in rest of table (for each j fill in i from top to bottom due to structure of table)
		// for all 0 <= i < j <= n
		for (j <- 2 to n) {
			for (i <- j-1 to 0 by -1) {
				// for all z
				for (x <- 0 to 2) {
					for (y <- 0 to 2) {
						var z = op(x)(y)

						for (k <- i to j) {
							poss(i)(j)(z) = poss(i)(j)(z) || (poss(i)(k)(x) && poss(k)(j)(y))

							// ways(i)(j)(z) += (ways(i)(k)(x) * ways(k)(j)(y))
							// to test when overflow occurs
							ways(i)(j)(z) = Math.addExact(ways(i)(j)(z), Math.multiplyExact(ways(i)(k)(x) , ways(k)(j)(y)))

							// for expressions
							if (poss(i)(j)(z) && exp(i)(j)(z) == null) {
								// splitting in [i..k) and [k..j) generates z
								exp(i)(j)(z) = Node(exp(i)(k)(x), exp(k)(j)(y))
							}
						}
					}
				}
			}
		}
	}

	//Task 6
	/* Runtime analysis of dp solution with tests

	The maximum word length for which my program correctly determines the number of bracketings is 20
  -> for words of length > 20 an integer overflow occurs -> can test with BBBBBBBBBBBBBBBBBBBBB (length 20) -> explanation below
  One can see that the sequence which will have the most possibilities is one of just B's, as any combination of B's gives a B.
    -> for a sequence of length n the number of possibilities is C(n-1) -> (n-1)th term of the Catalan numbers (see links below)
    -> the first Catalan number that overflows 32 bits (4294967295) is 6564120420 = C(20)
    -> therefore the maximum number of B's must correspond to C(19) => 20 B's 
    -> as all other sequences of length 20 have less possibilities they will also be possible => max length is 20
  Link for the Catalan numbers: https://oeis.org/A000108 (note C(n) is 0-indexed)
  Link for Catalan numbers wikipedia (go to number of parenthesis methods): https://en.wikipedia.org/wiki/Catalan_number
	
	The algorithm should take O(n^3) where n is the length of the input w
		-> the table is nxnx3 = 3n^2 and each entry in the table takes linear time to calculate
	Testing:
	on ABBA (n = 4) takes 0.72s
	on ABBAABBA (n = 8) takes 0.72s
	on ABBAABBAABBAABBA (n = 16) takes 0.72s
	on ABBAABBAABBAABBAABBA (n = 20) takes 0.72s
	after that overflow occurs so incorrect results generated
	This verifies the time complexity as we can only work on small values of n and with a quadratic time complexity near to no difference will be seen

	Compared to the recursive version it is way faster as expected 
		-> we are not repeating unecessary work
	*/
  

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
    if(command=="-PossibleRec"){
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
    else if(command=="-NumberRec"){
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

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }      
    else println(errString)
  }
}


