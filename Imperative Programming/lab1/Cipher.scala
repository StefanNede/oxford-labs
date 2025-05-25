object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] = {
    val textSize = plain.size
    val keySize = key.size
    val cipher = new Array[Char](textSize) // result array

    // encrypting the contents of plain, putting the result into cipher
    var i = 0 
    while (i < textSize) {
      cipher(i) = xor(plain(i), key(i%keySize))
      i += 1
    }

    cipher
  }

  /* 4. start question: 
    You could recover the key by xoring the crib (known characters) with the first encrypted characters 
    and getting back the key used for those characters (this works due to the cancellation law of xor)
  */

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) : Unit = {
    val cribLength = crib.size
    val keyChars = new Array[Char](cribLength)
    var start = 0

    // looping over all possible start values
    while (start < ciphertext.size - cribLength) {
      var i = 0
      // now do step 1 for this start (called currentStart)
      while (i < cribLength) {
        // find the necessary key-characters for the crib to appear at that position and store in keyChars
        keyChars(i) = xor(ciphertext(start + i), crib(i)) // via the cancellation law
        i += 1
      }

      // step 2
      var j = 1
      var foundRepetition = false
      while ((j <= cribLength - 2) && !foundRepetition) {
        foundRepetition = containsRepetition(keyChars, j)
        j += 1
      }

      // step 3
      if (foundRepetition) {
        j -= 1 // decrement it as was incremented on the index the repetition was found
        // j is now the length of the key
        var key:String = new String(keyChars)
        var missing = start%j // the number of characters missing from the front i.e. for UDOLFR it is one (R)

        key = key.slice(0, j) // get all the characters contained in the key
        key = key.slice(j-missing,j) + key.slice(0,j-missing) // rotate text j characters around i.e. move the last j characters to the front

        // print out key and decrypted cipher text
        println(key)
        print(new String(encrypt(key.toArray, ciphertext)))

        return // found the key so can stop the execution of the function
      }

      start += 1 // go onto next value of start
    }
    println("Couldn't decode the message with the given crib :(")
  }

  // checks whether at keyChars[0..K-j) = keyChars[j..K) (step 2)
  def containsRepetition(keyChars: Array[Char], j:Int) : Boolean = {
    var i = 0
    while (i < (keyChars.size - j)) {
      if (keyChars(i) != keyChars(j+i))  {
        return false
      }
      i += 1
    }
    return true
  }

  /** The first optional statistical test, to guess the length of the key */
  // matches due to the same character being encoded the same way will only occur when the shift is a multiple of the key-length
  // count the number of matches for every possible shift -> greatest when shift is a multiple of the key-length
  // for private1 gives highest match count for shift 36 then 18 then shift 9 -> probably a shift of 9
  // for private2 gives highest match count for shift 8
  def crackKeyLen(ciphertext: Array[Char]) : Unit = {
    // count number of matches for various shifts showing the counts 
    // try shift amounts from 1 to 40 inclusive
    println("Trying to find the key length")
    var shift = 1
    while (shift <= 40) {
      val shiftedtext = getShiftedText(ciphertext, shift)
      var count = getMatchCount(ciphertext, shiftedtext)
      println(shift + ": " + count)
      shift += 1
    }
  }

  def getMatchCount(ciphertext: Array[Char], shiftedtext: Array[Char]) : Int = {
    var count = 0
    var i = 0
    while (i < ciphertext.length) {
      if (ciphertext(i) == shiftedtext(i)) {
        count += 1
      }
      i += 1
    }
    return count
  }

  // to shift the ciphertext characters by the required shift
  def getShiftedText(ciphertext: Array[Char], shift: Int) : Array[Char] = {
    // need to make it wrap around
    var i = 0
    var n = ciphertext.length
    val shiftedtext = new Array[Char](n)
    // iterate over each character and place it into shiftedtext 
    while (i < n) {
      var newPos = (i + shift)%n
      shiftedtext(newPos) = ciphertext(i)
      i += 1
    }
    return shiftedtext
  }

  /** The second optional statistical test, to guess characters of the key. */
  // again use matches between the cipher-text and a shifted copy
  // use shifts that are multiples of guess key length
  // work out for each match what the corresponding key-character must be 
  // from position work out position of character in key

  // upon running scala Cipher -crackKey 9 private1 | sort -n | uniq -c | awk '$1 > 6' you get PEMBERLEY which is the correct key
  // upon running scala Cipher -crackKey 8 private2 | sort -n | uniq -c | awk '$1 > 6' you get HOGWARTS which is the correct key
  def crackKey(klen: Int, ciphertext: Array[Char]) : Unit = {
    // produce list of guessed key characters
    var s = 1 // multiple of klen
    // I will go up to 20 multiples of klen
    while (s <= 20) {
      var shift = klen*s
      val shiftedtext = getShiftedText(ciphertext, shift)

      // now go through matches between ciphertext and shiftedtext and xor with space character
      var i = 0
      while (i < ciphertext.length) {
        if (ciphertext(i) == shiftedtext(i)) {
          // xor with space character
          var keyChar = xor(ciphertext(i), ' ')
          // calculate position in key
          var keyPos = (i%klen)+1
          // suppressing characters that are not printable
          if (!(keyChar.toInt < 32 || keyChar.toInt > 127)) {
            println(keyPos + " " + keyChar)
          }
        }
        i += 1
      }
      s += 1
    }
  }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else readStdin()

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}
