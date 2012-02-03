//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.generator.*;
import com.startingblocktech.tcases.generator.io.*;
import com.startingblocktech.tcases.io.*;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}.
 *
 * @version $Revision$, $Date$
 */
public class Tcases
  {
  /**
   * Represents a set of command line options.
   *
   * Command line arguments have the following form.
   * <P/>
   * <BLOCKQUOTE>
   * <CODE>
   * <TABLE cellspacing="0" cellpadding="8">
   * <TR valign="top">
   * <TD colspan="3">
   * <NOBR>
   * [-c <I>tupleSize</I>]
   * [-f <I>outFile</I>]
   * [-g <I>genDef</I>]
   * [-n]
   * [-o <I>outDir</I>]
   * [-p <I>name</I>=<I>value</I>]
   * [-r <I>seed</I>]
   * [-t <I>testDef</I>]
   * [-x <I>transformDef</I> | -J]
   * [<I>inputDef</I>]
   * </NOBR>
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp; where:
   * </TD>
   * <TD>
   * </TD>
   * <TD>
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-c <I>tupleSize</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-c</I> is defined, use the given default <I>tupleSize</I> for all generators. This updates the generator definitions specified by the
   * <I>genDef</I> file.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-f <I>outFile</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-f</I> is defined, test definition output is written to the specified <I>outFile</I>, relative to the given <I>outDir</I>.
   * If omitted, test definitions are written to the file specified by the <I>-t</I> option.
   * If an output path cannot be derived, output is written to standard output.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-g <I>genDef</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-g</I> is defined, test definitions are created using the generator(s) specified
   * by the given <I>genDef</I> file. If omitted, the default generator definition is used.
   * The default generator definition is read from the corresponding <CODE>*-Generators.xml</CODE> file in the same directory as the <I>inputDef</I>,
   * if it exists. Otherwise, the default {@link TupleGenerator} is used for all functions.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-J </NOBR>
   * </TD>
   * <TD>
   * If <I>-J</I> is defined, test definition output is transformed into Java source code for a JUnit
   * test class. The resulting Java source file is written to the specified <I>outDir</I>. The
   * following parameters (see the <I>-p</I> option) affect the results of this transform.
   * <BLOCKQUOTE>
   * <TABLE border="0" cellspacing="16">
   * <TR valign="baseline">
   * <TD><B> class </B></TD>
   * <TD>
   * The name of the class under test. If omitted, the default is defined by the <B>system</B>
   * parameter.
   * </TD>
   * </TR>
   * <TR valign="baseline">
   * <TD><B> system </B></TD>
   * <TD>
   * The name of the system under test. If omitted, the default is defined by the <I>inputDef</I>.
   * </TD>
   * </TR>
   * <TR valign="baseline">
   * <TD><B> throws </B></TD>
   * <TD>
   * A boolean value (true/false, yes/no). If true, generated test methods are declared to throw Exception.
   * If omitted, the default is false.
   * </TD>
   * </TR>
   * </TABLE>   
   * </BLOCKQUOTE>
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-n </NOBR>
   * </TD>
   * <TD>
   * If <I>-n</I> is defined, any previous contents of the <I>testDef</I> are ignored.
   * If omitted, new test definitions are based on the previous <I>testDef</I>.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-o <I>outDir</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-o</I> is defined, test definition output is written to the specified directory.
   * If omitted, the default <I>outDir</I> is the directory containing the <I>inputDef</I> or,
   * if reading from standard input, the current working directory. If an output path cannot be
   * derived, output is written to standard output.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-p <I>name</I>=<I>value</I> </NOBR>
   * </TD>
   * <TD>
   * Defines the value of a transform parameter. Any number of <I>-p</I> options may be specified.
   * This option is meaningful only if the <I>-x</I> or <I>-J</I> option is given.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-r <I>seed</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-r</I> is defined, use the given random number <I>seed</I> for all generators. This updates the generator definitions specified by the
   * <I>genDef</I> file.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-x <I>transformDef</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-x</I> is defined, test definition output is transformed according to the XSLT transform defined
   * by the <I>transformDef</I> file. If relative, the <I>transformDef</I> path is assumed to be relative to the
   * directory containing the <I>inputDef</I>.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-t <I>testDef</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-t</I> is defined, new test definitions are based on the contents of the specified <I>testDef</I> file,
   * relative to the directory containing the <I>inputDef</I>. Also, unless the <I>-f</I> option is given, new
   * test definition output is written to the specified <I>testDef</I> path,
   * relative to the <I>outDir</I>.
   * If omitted, the default <I>testDef</I> name is derived from the <I>inputDef</I> name.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR><I>inputDef</I> </NOBR>
   * </TD>
   * <TD>
   * The system input definition is read from the given <I>inputDef</I>. If omitted, the system input definition is
   * read from standard input. Otherwise, the system input definition is read from the first one of the following files
   * that can be located.
   * <OL> 
   * <LI> <I>inputDef</I> </LI>
   * <LI> <I>inputDef</I>-Input.xml </LI>
   * <LI> <I>inputDef</I>.xml </LI>
   * </OL>
   * 
   * </TABLE>
   * </CODE>
   * </BLOCKQUOTE>
   * <P/> 
   *
   * @version $Revision$, $Date$
   */
  public static class Options
    {
    /**
     * Creates a new Options object.
     */
    public Options()
      {
      setExtended( true);
      setWorkingDir( null);
      }
    
    /**
     * Creates a new Options object.
     */
    public Options( String[] args)
      {
      this();
      
      int i;

      // Handle options
      for( i = 0; i < args.length && args[i].charAt(0) == '-'; i = handleOption( args, i));

      // Handle additional arguments.
      handleArgs( args, i);
      }

    /**
     * Handles the i'th option and return the index of the next argument.
     */
    protected int handleOption( String[] args, int i)
      {
      String arg = args[i];

      if( arg.equals( "-o"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setOutDir( new File( args[i]));
        }

      else if( arg.equals( "-f"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setOutFile( new File( args[i]));
        }

      else if( arg.equals( "-t"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setTestDef( new File( args[i]));
        }

      else if( arg.equals( "-g"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setGenDef( new File( args[i]));
        }

      else if( arg.equals( "-r"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          setRandomSeed( Long.valueOf( args[i]));
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid random seed", e);
          }
        }

      else if( arg.equals( "-c"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          setDefaultTupleSize( Integer.valueOf( args[i]));
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid tuple size", e);
          }
        }

      else if( arg.equals( "-n"))
        {
        setExtended( false);
        }

      else if( arg.equals( "-J"))
        {
        if( getTransformDef() != null)
          {
          throwUsageException( "Can't specify both -J and -x");
          }
        setJUnit( true);
        }

      else if( arg.equals( "-x"))
        {
        if( isJUnit())
          {
          throwUsageException( "Can't specify both -J and -x");
          }
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setTransformDef( new File( args[i]));
        }

      else if( arg.equals( "-p"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        String binding = args[i];
        int valuePos = binding.indexOf( '=');
        if( valuePos < 0)
          {
          throwUsageException( "Invalid -p option: must be name=value");
          }
        String name = StringUtils.trimToNull( binding.substring( 0, valuePos));
        if( name == null)
          {
          throwUsageException( "Invalid -p option: parameter name undefined");
          }
        String value = binding.substring( valuePos+1);
        getTransformParams().put( name, value);
        }
      
      else
        {
        throwUsageException();
        }
      
      return i + 1;
      }

    /**
     * Handles the non-option arguments i, i+1, ...
     */
    protected void handleArgs( String[] args, int i)
      {
      int nargs = args.length - i;

      if( nargs > 1)
        {
        throwUsageException();
        }

      if( nargs > 0)
        {
        setInputDef( new File( args[i]));
        }
      }

    /**
     * Throws a RuntimeException reporting a command line error.
     */
    protected void throwUsageException()
      {
      throwUsageException( null, null); 
      }

    /**
     * Throws a RuntimeException reporting a command line error.
     */
    protected void throwUsageException( String detail)
      {
      throwUsageException( detail, null);
      }

    /**
     * Throws a RuntimeException reporting a command line error.
     */
    protected void throwUsageException( String detail, Exception cause)
      {
      if( detail != null)
        {
        cause = new RuntimeException( detail, cause);
        }
      
      throw
        new RuntimeException
        ( "Usage: "
          + Tcases.class.getSimpleName()
          + " [-c tupleSize]"
          + " [-f outFile]"
          + " [-g genDef]"
          + " [-n]"
          + " [-o outDir]"
          + " [-p name=value]"
          + " [-r seed]"
          + " [-t testDef]"
          + " [-x transformDef | -J]"
          + " [inputDef]",
          cause);
      }

    /**
     * Changes the output directory for generated test definitions.
     */
    public void setOutDir( File outDir)
      {
      outDir_ = outDir;
      }

    /**
     * Returns the output directory for generated test definitions.
     */
    public File getOutDir()
      {
      return outDir_;
      }

    /**
     * Changes the output file for generated test definitions.
     */
    public void setOutFile( File outFile)
      {
      outFile_ = outFile;
      }

    /**
     * Returns the output file for generated test definitions.
     */
    public File getOutFile()
      {
      return outFile_;
      }

    /**
     * Changes the output file for generated test definitions.
     */
    public void setTestDef( File testDef)
      {
      testDef_ = testDef;
      }

    /**
     * Returns the output file for generated test definitions.
     */
    public File getTestDef()
      {
      return testDef_;
      }

    /**
     * Changes the generator definition file.
     */
    public void setGenDef( File genDef)
      {
      genDef_ = genDef;
      }

    /**
     * Returns the generator definition file.
     */
    public File getGenDef()
      {
      return genDef_;
      }

    /**
     * Changes the transform file.
     */
    public void setTransformDef( File transformDef)
      {
      transformDef_ = transformDef;
      }

    /**
     * Returns the transform file.
     */
    public File getTransformDef()
      {
      return transformDef_;
      }

    /**
     * Changes if using the JUnit transform.
     */
    public void setJUnit( boolean junit)
      {
      junit_ = junit;
      }

    /**
     * Returns if using the JUnit transform.
     */
    public boolean isJUnit()
      {
      return junit_;
      }

    /**
     * Changes the transform parameter bindings.
     */
    public void setTransformParams( Map<String,Object> params)
      {
      transformParams_ = params;
      }

    /**
     * Returns the transform parameter bindings.
     */
    public Map<String,Object> getTransformParams()
      {
      return transformParams_;
      }

    /**
     * Changes the input definition file
     */
    public void setInputDef( File inputDef)
      {
      inputDef_ = inputDef;
      }

    /**
     * Returns the input definition file
     */
    public File getInputDef()
      {
      return inputDef_;
      }

    /**
     * Changes if new test definitions are generated by extending the
     * previous {@link #getTestDef test definitions}.
     */
    public void setExtended( boolean extended)
      {
      extended_ = extended;
      }

    /**
     * Returns if new test definitions are generated by extending the
     * previous {@link #getTestDef test definitions}.
     */
    public boolean isExtended()
      {
      return extended_;
      }

    /**
     * Changes the random seed used by generators.
     */
    public void setRandomSeed( Long seed)
      {
      seed_ = seed;
      }

    /**
     * Returns the random seed used by generators.
     */
    public Long getRandomSeed()
      {
      return seed_;
      }

    /**
     * Changes the default tuple size used by generators.
     */
    public void setDefaultTupleSize( Integer tupleSize)
      {
      defaultTupleSize_ = tupleSize;
      }

    /**
     * Returns the default tuple size used by generators.
     */
    public Integer getDefaultTupleSize()
      {
      return defaultTupleSize_;
      }

    /**
     * Changes the current working directory used to complete relative path names.
     */
    public void setWorkingDir( File workingDir)
      {
      workingDir_ =
        workingDir == null
        ? new File( ".")
        : workingDir;
      }

    /**
     * Returns the current working directory used to complete relative path names.
     */
    public File getWorkingDir()
      {
      return workingDir_;
      }

    public String toString()
      {
      StringBuilder builder = new StringBuilder();

      if( getDefaultTupleSize() != null)
        {
        builder.append( " -c ").append( getDefaultTupleSize());
        }

      if( getOutFile() != null)
        {
        builder.append( " -f ").append( getOutFile().getPath());
        }

      if( getGenDef() != null)
        {
        builder.append( " -g ").append( getGenDef().getPath());
        }

      if( !isExtended())
        {
        builder.append( " -n");
        }

      if( getOutDir() != null)
        {
        builder.append( " -o ").append( getOutDir().getPath());
        }

      if( getTransformParams() != null)
        {
        for( String name : getTransformParams().keySet())
          {
          builder.append( " -p ").append( name).append( '=').append( getTransformParams().get( name));
          }
        }

      if( getRandomSeed() != null)
        {
        builder.append( " -r ").append( getRandomSeed());
        }

      if( getTestDef() != null)
        {
        builder.append( " -t ").append( getTestDef().getPath());
        }

      if( getTransformDef() != null)
        {
        builder.append( " -x ").append( getTransformDef().getPath());
        }

      if( isJUnit())
        {
        builder.append( " -J");
        }
        
      return builder.toString();
      }

    private File inputDef_;
    private File outDir_;
    private File outFile_;
    private File testDef_;
    private File genDef_;
    private File transformDef_;
    private Map<String,Object> transformParams_ = new HashMap<String,Object>();
    private boolean junit_;
    private boolean extended_;
    private Long seed_;
    private Integer defaultTupleSize_;
    private File workingDir_;
    }
  
  /**
   * Creates a new Tcases object.
   */
  public Tcases()
    {
    }

  /**
   * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}
   * using the given {@link Options command line options}.
   */
  public static void main( String[] args)
    {
    int exitCode = 0;
    try
      {
      Tcases tcases = new Tcases();
      tcases.run( new Options( args));
      }
    catch( Exception e)
      {
      exitCode = 1;
      e.printStackTrace( System.err);
      }
    finally
      {
      System.exit( exitCode);
      }
    }

  /**
   * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}
   * using the given {@link Options command line options}.
   */
  public void run( Options options) throws Exception
    {
    // Identify the system input definition file.
    File inputDefOption = options.getInputDef();
    if( inputDefOption != null && !inputDefOption.isAbsolute())
      {
      inputDefOption = new File( options.getWorkingDir(), inputDefOption.getPath());
      }

    File inputDefFile = inputDefOption;
    if( inputDefFile != null
        && !inputDefFile.exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".xml")).exists())
        {
        throw new RuntimeException( "Can't locate input file for path=" + options.getInputDef());
        }

    File inputDir =
      inputDefFile==null
      ? options.getWorkingDir()
      : inputDefFile.getParentFile();
    
    // Read the system input definition.
    SystemInputDef inputDef = null;
    InputStream inputStream = null;
    try
      {
      logger_.info( "Reading system input definition={}", inputDefFile);
      if( inputDefFile != null)
        {
        inputStream = new FileInputStream( inputDefFile);
        }
      SystemInputDocReader reader = new SystemInputDocReader( inputStream);
      inputDef = reader.getSystemInputDef();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read input definition file=" + inputDefFile, e);
      }
    finally
      {
      IOUtils.closeQuietly( inputStream);
      }

    // Identify the test definition file. 
    File testDefFile = options.getTestDef();
    File baseDefFile = null;

    File outputDir = options.getOutDir();
    File outputFile = options.getOutFile();
    if( outputFile == null)
      {
      outputFile = testDefFile;
      }
    
    if( !(inputDefFile == null && outputFile == null))
      {
      // Test definition defined?
      if( testDefFile == null)
        {
        // No, derive default from input file.
        String inputBase = FilenameUtils.getBaseName( inputDefFile.getName());

        String testDefBase =
          inputBase.toLowerCase().endsWith( "-input")
          ? inputBase.substring( 0, inputBase.length() - "-input".length())
          : inputBase;

        testDefFile = new File( testDefBase + "-Test.xml");
        }

      // For relative path, read base test definitions from input directory.
      baseDefFile = testDefFile;
      if( !baseDefFile.isAbsolute())
        {
        baseDefFile = new File( inputDir, baseDefFile.getPath());
        }

      // For relative path, write test definitions to output directory.
      if( outputFile == null)
        {
        outputFile = testDefFile;
        }
      
      if( outputDir == null)
        {
        outputDir =
          outputFile.isAbsolute()
          ? outputFile.getParentFile()
          : inputDir;
        }

      outputFile =
        new File
        ( outputDir,
          outputFile.isAbsolute()? outputFile.getName() : outputFile.getPath());

      // Ensure output directory exists.
      if( !outputDir.exists() && !outputDir.mkdirs())
        {
        throw new RuntimeException( "Can't create output directory=" + outputDir);
        }
      }
            
    SystemTestDef baseDef = null;
    if( options.isExtended() && baseDefFile != null && baseDefFile.exists())
      {
      // Read the previous base test definitions.
      InputStream testStream = null;
      try
        {
        logger_.info( "Reading base test definition={}", baseDefFile);
        testStream = new FileInputStream( baseDefFile);
        SystemTestDocReader reader = new SystemTestDocReader( testStream);
        baseDef = reader.getSystemTestDef();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read test definition file=" + baseDefFile, e);
        }
      finally
        {
        IOUtils.closeQuietly( testStream);
        }
      }

    // Identify the generator definition file.
    File genDefFile = options.getGenDef();
    File genDefDefault = null;
    if( genDefFile != null)
      {
      if( !genDefFile.isAbsolute())
        {
        genDefFile = new File( inputDir, genDefFile.getPath());
        }
      }
    else if( inputDefFile != null)
      {
      String inputBase = FilenameUtils.getBaseName( inputDefFile.getName());

      String genDefBase =
        inputBase.toLowerCase().endsWith( "-input")
        ? inputBase.substring( 0, inputBase.length() - "-input".length())
        : inputBase;

      genDefDefault = new File( inputDir, genDefBase + "-Generators.xml");
      if( genDefDefault.exists())
        {
        genDefFile = genDefDefault;
        }
      }
      
    // Generator definitions specified?
    IGeneratorSet genDef = null;
    if( genDefFile != null)
      {
      // Yes, read generator definitions.
      InputStream genStream = null;
      try
        {
        logger_.info( "Reading generator definition={}", genDefFile);
        genStream = new FileInputStream( genDefFile);
        GeneratorSetDocReader reader = new GeneratorSetDocReader( genStream);
        genDef = reader.getGeneratorSet();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read generator definition file=" + genDefFile, e);
        }
      finally
        {
        IOUtils.closeQuietly( genStream);
        }
      }
    else
      {
      // No, use default TupleGenerator.
      GeneratorSet genSet = new GeneratorSet();
      genSet.addGenerator( GeneratorSet.ALL, new TupleGenerator());
      genDef = genSet;
      } 

    // Generate new test definitions.
    SystemTestDef testDef = new SystemTestDef( inputDef.getName());
    for( Iterator<FunctionInputDef> functionDefs = inputDef.getFunctionInputDefs(); functionDefs.hasNext();)
      {
      FunctionInputDef functionDef = functionDefs.next();
      FunctionTestDef functionBase = baseDef==null? null : baseDef.getFunctionTestDef( functionDef.getName());
      TupleGenerator functionGen = (TupleGenerator) genDef.getGenerator( functionDef.getName());
      if( functionGen == null)
        {
        throw new RuntimeException( "No generator for function=" + functionDef.getName() + " defined in generator definition=" + genDefFile);
        }

      if( options.getRandomSeed() != null)
        {
        functionGen.setRandomSeed( options.getRandomSeed());
        }
      if( options.getDefaultTupleSize() != null)
        {
        functionGen.setDefaultTupleSize( options.getDefaultTupleSize());
        }
      
      testDef.addFunctionTestDef( functionGen.getTests( functionDef, functionBase));
      }

    // Identify test definition transformations.
    TransformFilter transformer = null;
    File transformDefFile = options.getTransformDef();
    if( transformDefFile != null)
      {
      if( !transformDefFile.isAbsolute())
        {
        transformDefFile = new File( inputDir, transformDefFile.getPath());
        }
      transformer = new TransformFilter( transformDefFile);
      }
    else if( options.isJUnit())
      {
      transformer = new TestDefToJUnitFilter();
      }
    
    if( transformer != null)
      {
      if( outputFile != null && outputFile.equals( baseDefFile) && baseDefFile.exists())
        {
        throw new RuntimeException( "Transformed output will overwrite test definition file=" + baseDefFile);
        }
      transformer.setTarget( outputFile);
      transformer.setParams( options.getTransformParams());
      }

    // Write new test definitions.
    SystemTestDocWriter writer = null;
    try
      {
      logger_.info( "Updating test definition file={}", outputFile);

      OutputStream output =
        // Transformed output?
        transformer != null?
        transformer.getSource() :

        // Output file?
        outputFile != null?
        new FileOutputStream( outputFile) :
        
        // Standard output?
        null;        
        
      writer = new SystemTestDocWriter( output);
      writer.write( testDef);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write test definition file=" + outputFile, e);
      }
    finally
      {
      if( writer != null)
        {
        writer.close();
        }
      }

    // Write any updates to generator definitions.
    File genUpdateFile =
      genDefFile != null?
      genDefFile :

      genDefDefault != null?
      genDefDefault :

      null;

    if( genUpdateFile != null && (options.getRandomSeed() != null || options.getDefaultTupleSize() != null))
      {
      GeneratorSetDocWriter genWriter = null;
      try
        {
        logger_.info( "Updating generator definition={}", genUpdateFile);
        genWriter = new GeneratorSetDocWriter( new FileOutputStream( genUpdateFile));
        genWriter.write( genDef);
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't write generator definition file=" + genUpdateFile, e);
        }
      finally
        {
        if( genWriter != null)
          {
          genWriter.close();
          }
        }
      }
    }

  private static final Logger logger_ = LoggerFactory.getLogger( Tcases.class);
  }
