import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.{Block,ControlStructure,Call,Identifier,Literal,Method,MethodReturn,Return,StoredNode}
import scala.collection.mutable.{HashMap,Queue}

def getNode(node:StoredNode): String = {
    node match {
        case b: Block =>
          b.code
        case cs: ControlStructure =>
          cs.code
        case c: Call =>
          c.code
        case id: Identifier =>
          id.code
        case l: Literal =>
          l.code
        case m: Method =>
          m.code
        case mr: MethodReturn =>
          mr.code
        case r: Return =>
          r.code
        case _ =>
          "Unknown node type"
      }
}


class CfgGenerator {

  val edgeType: String = EdgeTypes.CFG

  def generate(methodNode: Method): Graph = {
    val vertices          = methodNode.cfgNode.l ++ List(methodNode, methodNode.methodReturn) ++ methodNode.parameter.l
    val verticesToDisplay = vertices.filter(cfgNodeShouldBeDisplayed)

    def edgesToDisplay(srcNode: StoredNode, visited: List[StoredNode] = List()): List[Edge] = {
      if (visited.contains(srcNode)) {
        List()
      } else {
        val children             = expand(srcNode).filter(x => vertices.contains(x.dst))
        val (visible, invisible) = children.partition(x => cfgNodeShouldBeDisplayed(x.dst))
        visible.toList ++ invisible.toList.flatMap { n =>
          edgesToDisplay(n.dst, visited ++ List(srcNode)).map(y => Edge(srcNode, y.dst, edgeType = edgeType))
        }
      }
    }

    val edges = verticesToDisplay.flatMap { v =>
      edgesToDisplay(v)
    }.distinct

    val allIdsReferencedByEdges = edges.flatMap { edge =>
      Set(edge.src.id, edge.dst.id)
    }

    Graph(
      verticesToDisplay
        .filter(node => allIdsReferencedByEdges.contains(node.id)),
      edges
    )
  }

  protected def expand(v: StoredNode): Iterator[Edge] =
    v._cfgOut.map(node => Edge(v, node, edgeType = edgeType))

  private def isConditionInControlStructure(v: StoredNode): Boolean = v match {
    case id: Identifier => id.astParent.isControlStructure
    case _              => false
  }

  private def cfgNodeShouldBeDisplayed(v: StoredNode): Boolean =
    isConditionInControlStructure(v) ||
      !(v.isInstanceOf[Literal] ||
        v.isInstanceOf[Identifier] ||
        v.isInstanceOf[Block] ||
        v.isInstanceOf[ControlStructure] ||
        v.isInstanceOf[JumpTarget] ||
        v.isInstanceOf[MethodParameterIn])

}

def findNextStatement(
    currentNode: StoredNode,
    nameHashMap: HashMap[StoredNode, String],
    hashMap: HashMap[StoredNode, List[StoredNode]],
    specificOperator: Option[String] = None
): List[StoredNode] = {

  val operators = Set("=", "==", ">=", "<=", ">", "<", "!=")

  def findValidNodes(nodes: List[StoredNode], acc: List[StoredNode]): List[StoredNode] = {
    nodes match {
      case Nil => acc
      case head :: tail =>
        val name = nameHashMap.get(head)

        if (name.exists(n => n == "RET")) {
          return List.empty[StoredNode]
        }

        if (name.exists(n => operators.exists(op => n.contains(op)))) {
          if (specificOperator.forall(op => name.exists(n => n.contains(op)))) {
            findValidNodes(tail, head :: acc)
          } else {
            findValidNodes(tail, acc)
          }
        } else {
          val nextNodes = hashMap.get(head).getOrElse(List.empty)
          findValidNodes(nextNodes, acc)
        }
    }
  }

  val initialNodes = hashMap.get(currentNode).getOrElse(List.empty)
  findValidNodes(initialNodes, List.empty).reverse
}


def findStartNode(nameHashMap:HashMap[StoredNode,String],hashMap:HashMap[StoredNode,  List[StoredNode]]):StoredNode = {
  val emptyNodeOpt = nameHashMap.find { case (_, value) => value == "<empty>" }
  emptyNodeOpt match {
    case Some((node, _)) => node
  }
}

def generateHashMap(
    currentNode: StoredNode,
    nameHashMap:HashMap[StoredNode,String],
    hashMap1:HashMap[StoredNode,  List[StoredNode]],
    hashMap:HashMap[StoredNode, List[StoredNode]]
): HashMap[StoredNode, List[StoredNode]] = {
  val nextNodes = findNextStatement(currentNode,nameHashMap,hashMap1)
  hashMap(currentNode) = nextNodes
  nextNodes.foreach{ node =>
    generateHashMap(node,nameHashMap, hashMap1,hashMap)
  }
  hashMap
}

def getLineNumber(node:StoredNode): Int = {
    node match {
        case b: Block =>
          return b.lineNumber.getOrElse(-1)
        case cs: ControlStructure =>
          return cs.lineNumber.getOrElse(-1)
        case c: Call =>
          return c.lineNumber.getOrElse(-1)
        case id: Identifier =>
          return id.lineNumber.getOrElse(-1)
        case l: Literal =>
          return l.lineNumber.getOrElse(-1)
        case m: Method =>
          return m.lineNumber.getOrElse(-1)
        case mr: MethodReturn =>
          return mr.lineNumber.getOrElse(-1)
        case r: Return =>
          return r.lineNumber.getOrElse(-1)
        case _ =>
            return -1
      }
}

def getOrder(
  list: List[StoredNode],
  hashMap: HashMap[StoredNode, List[StoredNode]]
): List[StoredNode] = {
  var result: List[StoredNode] = List()
  val visited = scala.collection.mutable.Set[StoredNode]()

  def traverse(node: StoredNode): Unit = {
    if (visited.contains(node) || !list.contains(node)) return
    visited.add(node)

    hashMap.get(node) match {
      case Some(nextNodes) =>
        nextNodes.foreach(traverse)
      case None => 
    }

    result = node :: result
  }

  list.foreach(traverse)
  result.distinct
}


def getWorklist(
  nodes:List[StoredNode],
  hashMap: HashMap[StoredNode, List[StoredNode]]
): List[StoredNode] = {
  val groupedByLineNumber: Map[Int, List[StoredNode]] = nodes.groupBy(node => getLineNumber(node))
  val sortedGroups: List[List[StoredNode]] = groupedByLineNumber.toList.sortBy(_._1).map(_._2)
  val workList: List[StoredNode] = sortedGroups.flatMap {
    case singleElement :: Nil => singleElement :: Nil
    case multipleElements => getOrder(multipleElements,hashMap)
  }
  workList
}

def getDoubleSide(input: String): (String, List[String]) = {
  val pattern = """(\w+)\s*([=><!]=?|[+\-*/]=?)\s*(.*)""".r
  if (input.trim == "<empty>") {
    return ("",List())
  }

  input match {
    case pattern(left, _, right) =>
      val rightParts = right.replace("(", "").replace(")", "")
                            .split(",")
                            .map(_.trim)
                            .toList
      (left, rightParts)
    case _ =>
      throw new IllegalArgumentException(s"Invalid input: $input")
  }
}

def getNumberHashMap(
    nameHashMap:HashMap[StoredNode,String],
    hashMap:HashMap[StoredNode, List[StoredNode]]
): (HashMap[String, Int],Int) = {
  var numberHashMap: HashMap[String, Int] = HashMap()
  var num = 0
  hashMap.keys.foreach { node =>
    nameHashMap.get(node) match {
      case Some(name) =>
        getDoubleSide(name).match {
          case (left, rightParts) =>
            if(left == ""){

            }else if (!numberHashMap.contains(left)) {
              numberHashMap += (left -> num)
              num += 1
            }
        }
      case None =>
    }
  }
  (numberHashMap,num)
}

def createHashMapWithLists(nodes: Iterable[StoredNode], num: Int): HashMap[StoredNode, List[List[String]]] = {
  nodes.foldLeft(HashMap[StoredNode, List[List[String]]]()) { (acc, node) =>
    val lists = List.fill(num)(List[String]())
    acc + (node -> lists)
  }
}

def updateList(
  statement:String,
  numberHashMap: HashMap[String, Int],
  numberList: HashMap[StoredNode, List[List[String]]],
  currentNode: StoredNode
):Unit = {
  val list = numberList(currentNode)
  getDoubleSide(statement).match{
    case (left,rightParts) =>
      if(left != ""){
        val n = numberHashMap(left)
        val newList = list.updated(n,rightParts)
        numberList(currentNode) = newList
      }
  }
}

def mergeList(
  node: StoredNode,
  nextNode:StoredNode,
  numberList: HashMap[StoredNode, List[List[String]]]
): Unit = {
  val oldList = numberList(node)
  val newList = numberList(nextNode)
  val mergeList =  (oldList zip newList).map{
    case (l1, l2) => (l1 ++ l2).distinct
  }
  numberList(nextNode) = mergeList
}

def checkCondition(
  currentNode:StoredNode,
  nameHashMap:HashMap[StoredNode,String],
  hashMap:HashMap[StoredNode,  List[StoredNode]],
  numberList: HashMap[StoredNode, List[List[String]]],
  numberHashMap: HashMap[String, Int]
): Int = {
  val statement = nameHashMap(currentNode)
  var leftNumber = 0
  var rightNumber = 0
  // if (statement.contains(">=")){
  //   val (left,rightParts) = getDoubleSide(nameHashMap(currentNode))
  //   val list = numberList(currentNode)
  //   val compareValueIndex = numberHashMap(left)
  //   list(compareValueIndex).foreach{ num =>

  //   }
  // }else if (statement.contains("<=")){
    
  // }else if (statement.contains("==")){
    
  // }else if (statement.contains("!=")){
    
  // }else if (statement.contains(">")) {
    
  // }else if (statement.contains("<")){
    
  // }else {
    
  // }

  val result =   if (leftNumber + rightNumber != 0) leftNumber + rightNumber else 3
  result
}

def conditionalConstant(
  startNode:StoredNode,
  nameHashMap:HashMap[StoredNode,String],
  hashMap:HashMap[StoredNode,  List[StoredNode]]
):(HashMap[StoredNode, List[List[String]]],HashMap[String, Int]) = {
  val result = HashMap[String, Set[String]]()
  val executableHashMap: HashMap[StoredNode, Boolean] = HashMap()
  val visitedHashMap: HashMap[StoredNode, Boolean] = HashMap()
  val keysList: List[StoredNode] = hashMap.keys.toList
  var workList: List[StoredNode] = getWorklist(keysList,hashMap)
  val (numberHashMap, num) = getNumberHashMap(nameHashMap, hashMap)
  val numberList = createHashMapWithLists(hashMap.keys,num)

  workList.foreach { node =>
    executableHashMap += (node -> false)
    visitedHashMap += (node -> false)
  }
  executableHashMap(workList.head) = true
  
  workList.foreach{ node =>
    if(executableHashMap(node)){
      if(nameHashMap(node) == "z = a + b")
      println(hashMap(node).length)
      if(hashMap(node).length == 1){
        updateList(nameHashMap(node),numberHashMap,numberList,node)
        val nextNode = hashMap(node).head
        executableHashMap(nextNode) = true
        mergeList(node,nextNode,numberList)
      }else if(hashMap(node).length == 0){
        updateList(nameHashMap(node),numberHashMap,numberList,node)
      }else{
        checkCondition(node,nameHashMap,hashMap,numberList,numberHashMap).match{
          case 1 =>
            val nextNode = hashMap(node).head
            executableHashMap(nextNode) = true
            mergeList(node,nextNode,numberList)
          case 2 =>
            val nextNode = hashMap(node)(1)
            executableHashMap(nextNode) = true
            mergeList(node,nextNode,numberList)
          case 3 =>
            hashMap(node).foreach{ nextNode =>
              executableHashMap(nextNode) = true
              mergeList(node,nextNode,numberList)
            }

        }
      }
    }
  }
  
  // numberHashMap.foreach{ node=>
  //   println(node)
  // }

  // workList.foreach{node =>
  //   println(nameHashMap(node))
  //   println(numberList(node))
  // }

  val lastNode = workList.last
  val resultList = numberList(lastNode)

  numberHashMap.foreach{(key,value) =>
    println(s"$key -> ${resultList(value)}")
  }
    
  (numberList,numberHashMap)
}

@main def exec(cpgFile: String, outFile: String) = {
    importCode(inputPath = cpgFile, projectName = "test")
    val method = cpg.method("main").next()
    val cfg = new CfgGenerator().generate(method)
    val hashMap1 = HashMap[StoredNode,  List[StoredNode]]()
    val nameHashMap = HashMap[StoredNode,String]()

    cfg.edges.foreach{edge =>
      val srcNode = edge.src
      nameHashMap.get(srcNode) match{
        case Some(src) =>
        case None =>
          nameHashMap(srcNode) = getNode(srcNode)
      }

      hashMap1.get(srcNode) match {
        case Some(nodes) =>
          val updateNodes = nodes :+ edge.dst
          hashMap1(srcNode) = updateNodes
        case None =>
          val newValue = List(edge.dst)
          hashMap1(srcNode) = newValue
        }
    }

    val startNode:StoredNode = findStartNode(nameHashMap,hashMap1)
    val newHashMap = HashMap[StoredNode,  List[StoredNode]]()
    val hashMap = generateHashMap(startNode,nameHashMap,hashMap1,newHashMap)
    val (numberList,numberHashMap) = conditionalConstant(startNode,nameHashMap,hashMap)
    // numberList.foreach{(key,resultList) =>
    //   println(nameHashMap(key))
    //   numberHashMap.foreach{(key,value) =>
    //     println(s"$key -> ${resultList(value)}")
    //   }


    // }
}