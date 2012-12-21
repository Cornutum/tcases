#######################################################################
#
#                     Copyright 2012, Cornutum Project
#                              www.cornutum.org
# 
#                         $Revision$
#      $Date$
# 
#######################################################################

script=`basename $0`
pgm=`basename $0 .sh`

usage()
{
  echo Usage: $script "[-c tupleSize] [-f outFile] [-g genDef] [-n] [-l logFile] [-o outDir] [-p name=value] [-r seed] [-t testDef] [-v] [-x transformDef | -J] [inputDef]" >&2
  echo "" >&2
  echo "  Generates a set of test cases from a system input definition, according" >&2
  echo "  to the given command line arguments." >&2
  echo "" >&2
  echo "  inputDef    The system input definition is read from the given inputDef. If omitted," >&2
  echo "              the system input definition is read from standard input. Otherwise, the" >&2
  echo "              system input definition is read from the first one of the following files" >&2
  echo "              that can be located." >&2
  echo "" >&2
  echo "                1. inputDef" >&2
  echo "                2. inputDef-Input.xml" >&2
  echo "                3. inputDef.xml" >&2
  echo "" >&2
  echo "  -c tuples   If -c is defined, use the given default tuple size for all generators." >&2
  echo "              This updates the generator definitions specified by the genDef file." >&2
  echo "" >&2
  echo "  -f outFile  If -f is defined, test definition output is written to the specified" >&2
  echo "              outFile, relative to the given outDir. If omitted, test definitions are" >&2
  echo "              written to the file specified by the -t option. If an output path cannot" >&2
  echo "              be derived, output is written to standard output." >&2
  echo "" >&2
 	echo "  -g genDef   If -g is defined, test definitions are created using the generator(s)" >&2
  echo "              specified by the given genDef file. If omitted, the default generator" >&2
  echo "              definition is used. The default generator definition is read from the" >&2
  echo "              corresponding *-Generators.xml file in the same directory as the inputDef," >&2
  echo "              if it exists. Otherwise, the default TupleGenerator is used for all" >&2
  echo "              functions." >&2
  echo "" >&2
  echo "  -J          If -J is defined, test definition output is transformed into Java source" >&2
  echo "              code for a JUnit test class. The resulting Java source file is written to" >&2
  echo "              the specified outDir." >&2
  echo "" >&2
  echo "  -l logFile  If -l is defined, log output is written to the given file. If omitted," >&2
  echo "              log output is written to a file named ${pgm}.log in the current working" >&2
  echo "              directory" >&2
  echo "" >&2
  echo "  -n          If -n is defined, any previous contents of the testDef are ignored." >&2
  echo "              If omitted, new test definitions are based on the previous testDef." >&2
  echo "" >&2
	echo "  -o outDir   If -o is defined, test definition output is written to the specified" >&2
  echo "              directory. If omitted, the default outDir is the directory containing" >&2
  echo "              the inputDef or, if reading from standard input, the current working" >&2
  echo "              directory. If an output path cannot be derived, output is written to" >&2
  echo "              standard output." >&2
  echo "" >&2
  echo "  -p name=value  Defines the value of a transform parameter. Any number of -p options" >&2
  echo "              may be specified. This option is meaningful only if the -x or -J option is given." >&2
  echo "" >&2
  echo "  -r seed     If -r is defined, use the given random number seed for all generators." >&2
  echo "              This updates the generator definitions specified by the genDef file." >&2
  echo "" >&2
 	echo "  -t testDef  If -t is defined, test definition output is written to the specified" >&2
  echo "              testDef path, relative to the outDir. If omitted, the default testDef" >&2
  echo "              name is derived from the inputDef name. If an output path cannot be" >&2
  echo "              derived, output is written to standard output." >&2
  echo "" >&2
 	echo "  -v          Shows the current Tcases version. If this option is given, no other" >&2
  echo "              action is performed." >&2
  echo "" >&2
  echo "  -x xltDef   If -x is defined, test definition output is transformed according to the" >&2
  echo "              XSLT transform defined by the xltDef file. If relative, the xltDef path is" >&2
  echo "              assumed to be relative to the directory containing the inputDef." >&2
}

while [ $# -gt 0 ] ; do
  case $1 in
    -c) shift; defTupleSize="$1";;
    -f) shift; outFile="$1";;
    -g) shift; genDef="$1";;
    -J) isJUnit="$1";;
    -l) shift; logFile="$1";;
    -n) noExtend="$1";;
    -o) shift; outDir="$1";;
    -p) shift; params="$params -p $1";;
    -r) shift; seed="$1";;
    -t) shift; testDef="$1";;
    -v) showVersion="$1";;
    -x) shift; xsltDef="$1";;
    -*) usage; exit 1;;
     *) break;;
  esac
  shift
done

logFile=${logFile:-${pgm}.log}
inputDef="$1"
shift

if [ $# -ne 0 ] ; then
  usage; exit 1
fi

binDir=`dirname $0`
releaseDir=`cd $binDir/..; pwd`
libDir=${releaseDir}/lib

classPath=${libDir}
for jar in ${libDir}/*.jar; do
  classPath="${classPath}:${jar}"
done

uname=`uname`
cygwin=`expr $uname : 'CYGWIN'`
if [ $cygwin -gt 0 ] ; then
  classPath=`cygpath --path -m "$classPath"`
  genDef=${genDef:+`cygpath -m "$genDef"`}
  inputDef=${inputDef:+`cygpath -m "$inputDef"`}
  logFile=`cygpath -m "$logFile"`
  outDir=${outDir:+`cygpath -m "$outDir"`}
  outFile=${outFile:+`cygpath -m "$outFile"`}
  testDef=${testDef:+`cygpath -m "$testDef"`}
  xsltDef=${xsltDef:+`cygpath -m "$xsltDef"`}
fi


java \
  -cp "$classPath" \
  -Dtcases.log.file="$logFile" \
  org.cornutum.tcases.Tcases \
  $showVersion \
  ${genDef:+-g "$genDef"} \
  ${seed:+-r "$seed"} \
  ${defTupleSize:+-c "$defTupleSize"} \
  $noExtend \
  $isJUnit \
  ${outDir:+-o "$outDir"} \
  ${outFile:+-f "$outFile"} \
  ${params} \
  ${testDef:+-t "$testDef"} \
  ${xsltDef:+-x "$xsltDef"} \
  ${inputDef:+"$inputDef"}

