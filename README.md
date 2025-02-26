# constant-propagation
To use this, you can enter "joern --script test1.sc --param cpgFile=/home/yang/Desktop/summer/scalpel/Scalpel/tests/test-cases/constant_propagation/tuples.py" in your terminal. The file can be change to the corresponding location.

The test1.sc will get the constant propagation of main function, it can be changed in exec function.

The nameHashMap store the code of corresponding node. 

The hashMap store the successors of the node. 

The workList store the node in program order. 

The numberHashMap own the location of a variable in List[List[String]]. For example, a corresponds to the third List[String]. 

The numberList stores the corresponding List[List[String]] of a node.

The program will automatically print the final constant. If the constant in the middle is needed, you can find it in workList or the key of numberList and get the corresponding List[List[String]] in numberList and then print it using printConstant function.

The checkCondition function is not yet complete and still needs to wait.

Using python3 traverse.py and then input the folder and the output location to do constant propagation.