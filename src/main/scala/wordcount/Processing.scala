package wordcount

class Processing {
   
  /**********************************************************************************************
   *
   *                          Aufgabe 1
   *   
   *********************************************************************************************
  */
  def getWords(line:String):List[String]={
    /*
     * Extracts all words from a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    val words = line.replaceAll( "[^A-Za-z]"," " ).toLowerCase()
    words.split("\\s+").filter(!_.isEmpty).toList
  }
  
  def getAllWords(l:List[(Int,String)]):List[String]={
    /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     *
     * Hint: Use the flatMap function
     */
    l.flatMap(x => getWords(x._2))
  }
  
  def countWords(l:List[String]):List[(String,Int)]={
    /*
     *  Gets a list of words and counts the occurrences of the individual words
     */
    l.groupBy(x => x).mapValues(_.size).toList
    //l.map(x => (x,1)).foldLeft
      //.foldLeft(List[(String, Int)]){(a,b)=> if a.equals(_) b + 1}
  }

  /**********************************************************************************************
   *
   *                          Aufgabe 2
   *
   *********************************************************************************************
  */

  def mapReduce[S,B,R](mapFun:(S=>B),
      redFun:(R,B)=>R,
      base:R,
      l:List[S]):R =

  l.map(mapFun).foldLeft(base)(redFun)

  def mapFun(s: String): (String,Int) = (s,1)

  def redFun(l: List[(String, Int)], v:(String,Int)): List[(String,Int)] = {
   // case _ if v._1.equals()

    //v::l
    //case _  if v._1.e
    //case _ if (for (x <- l) yield l(x)._1 == v._1)
    ???
  }

  def countWordsMR(l: List[String]): List[(String, Int)] = {
  mapReduce[String,(String,Int),List[(String, Int)]](mapFun, redFun,List(),l)
  }
  
  
  /**********************************************************************************************
   *
   *                          Aufgabe 3
   *
   *********************************************************************************************
  */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    l.flatMap(x => getWords(x._2).map(y => (x._1,y)))
  }

  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
    l.map(x => (x._2,x._1)).groupBy(x => x._1).mapValues(x => x.map(x => x._2))
  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    invInd.filterKeys(x => words.contains(x)).values.flatten.toList.distinct
  }

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    invInd.filterKeys(x => words.contains(x)).values.toList.flatten.groupBy(identity).filter(x => x._2.length == words.length).keys.toList.sorted
  }
}


object Processing{
  
  def getData(filename:String):List[(Int,String)]={
    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }
}