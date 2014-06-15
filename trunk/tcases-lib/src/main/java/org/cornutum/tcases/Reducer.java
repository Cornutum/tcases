//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2014, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.generator.io.*;
import org.cornutum.tcases.io.*;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Random;

/**
 * For a {@link SystemInputDef system input definition}, updates the associated {@link GeneratorSet test case generators}
 * to reduce the number of generated test cases.
 *
 * @version $Revision: 268 $, $Date: 2014-05-04 14:10:36 -0500 (Sun, 04 May 2014) $
 */
public class Reducer
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
   * [-f <I>function</I>]
   * [-g <I>genDef</I>]
   * [-r <I>resampleFactor</I>]
   * [-s <I>sampleCount</I>]
   * [-t <I>baseTestDef</I>]
   * <I>inputDef</I>
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
   * <NOBR>-f <I>function</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-f</I> is defined, update only the test case generator for the given function. Otherwise, update the test case generators for all functions.
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
   * If <I>-g</I> is defined, update the generator specified in the given <I>genDef</I> file. Otherwise, update the default generate definition file:
   * the corresponding <CODE>*-Generators.xml</CODE> file in the same directory as the <I>inputDef</I>.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-r <I>resampleFactor</I></NOBR>
   * </TD>
   * <TD>
   * If <I>-r</I> is defined, use the given <I>resampleFactor</I> to determine the number of samples in the next round of reducing.
   * Depending on the <I>resampleFactor</I>, the next round may use more or fewer samples. If the previous round
   * called for <CODE>N</CODE> samples and produced a reduction, then the number of samples for the next round will be
   * <CODE>N * ( 1 + resampleFactor)</CODE>. To increase sample count with each round, define <I>resampleFactor</I> &gt; 0.
   * To decrease sample count with each round, define -1 &lt; <I>resampleFactor</I> &lt; 0. If <I>resampleFactor</I> is omitted,
   * the default value is 0.
   * </TD>
   * </TR>
   * 
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-s <I>sampleCount</I> </NOBR>
   * </TD>
   * <TD>
   * Defines the number of samples for the initial round of reducing.
   * If omitted, the default <I>sampleCount</I> is 10.
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
   * If <I>-t</I> is defined, generate test cases based on the test definitions in the specified <I>testDef</I> file,
   * relative to the directory containing the <I>inputDef</I>.
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
   * The system input definition is read from the first one of the following files
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
   * @version $Revision: 268 $, $Date: 2014-05-04 14:10:36 -0500 (Sun, 04 May 2014) $
   */
  public static class Options
    {
    /**
     * Creates a new Options object.
     */
    public Options()
      {
      setSamples( 10);
      setResampleFactor( 0.0);
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

      if( arg.equals( "-f"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setFunction( args[i]);
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
          setResampleFactor( Double.parseDouble( args[i]));
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid resample factor", e);
          }
        }

      else if( arg.equals( "-s"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          int samples = Integer.parseInt( args[i]);
          if( samples <= 0)
            {
            throw new IllegalArgumentException( "Sample count must be greater than 0");
            }
          setSamples( samples);
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid sample count", e);
          }
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
      if( nargs != 1)
        {
        throwUsageException();
        }

      setInputDef( new File( args[i]));
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
          + Reducer.class.getSimpleName()
          + " [-f function]"
          + " [-g genDef]"
          + " [-r resampleFactor]"
          + " [-s sampleCount]"
          + " [-t testDef]"
          + " inputDef",
          cause);
      }

    /**
     * Changes the initial number of samples.
     */
    public void setSamples( int samples)
      {
      samples_ = samples;
      }

    /**
     * Returns the initial number of samples.
     */
    public int getSamples()
      {
      return samples_;
      }

    /**
     * Changes the function for which tests cases are reduced.
     */
    public void setFunction( String function)
      {
      function_ = function;
      }

    /**
     * Returns the function for which tests cases are reduced.
     */
    public String getFunction()
      {
      return function_;
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
     * Changes the resample factor.
     * The <CODE>resampleFactor</CODE> determines the number of samples in the next round of reducing.
     * Depending on the <CODE>resampleFactor</CODE>, the next round may use more or fewer samples.
     * <P/>
     * If the previous round called for <CODE>N</CODE> samples and produced a reduction, then the number of samples for the
     * next round will be <CODE>N * ( 1 + resampleFactor)</CODE>. To increase sample count with each round, define
     * <CODE>resampleFactor</CODE> &gt; 0.  To decrease sample count with each round, define -1 &lt;
     * <CODE>resampleFactor</CODE> &lt; 0.
     */
    public void setResampleFactor( double resampleFactor)
      {
      resampleFactor_ = resampleFactor;
      }

    /**
     * Returns the {@link setResampleFactor resample factor}.
     */
    public double getResampleFactor()
      {
      return resampleFactor_;
      }

    public String toString()
      {
      StringBuilder builder = new StringBuilder();

      if( getFunction() != null)
        {
        builder.append( " -f ").append( getFunction());
        }

      if( getGenDef() != null)
        {
        builder.append( " -g ").append( getGenDef().getPath());
        }

      builder.append( " -r ").append( getResampleFactor());
      builder.append( " -s ").append( getSamples());

      if( getTestDef() != null)
        {
        builder.append( " -t ").append( getTestDef().getPath());
        }
        
      return builder.toString();
      }

    private File inputDef_;
    private String function_;
    private File testDef_;
    private File genDef_;
    private double resampleFactor_;
    private int samples_;
    }
  
  /**
   * Creates a new Reducer object.
   */
  public Reducer()
    {
    }

  /**
   * For a {@link SystemInputDef system input definition}, updates the associated {@link GeneratorSet test case generators}
   * to reduce the number of generated test cases, using the given {@link Options command line options}.
   * <P/>
   * @see Reducer#run
   */
  public static void main( String[] args)
    {
    int exitCode = 0;
    try
      {
      Reducer reducer = new Reducer();
      reducer.run( new Options( args));
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
   * For a {@link SystemInputDef system input definition}, updates the associated {@link GeneratorSet test case generators}
   * to reduce the number of generated test cases, using the given {@link Options command line options}.
   * <P/>
   * The reducing process operates as a sequence of "rounds". Each round consists of a series of test case generations executions
   * called "samples". Each sample uses a new random seed to generate test cases for a specified function in
   * an attempt to find a seed that produces the fewest test cases.
   * <P/>
   * If all samples in a round complete without reducing the current minimum test case count, the reducing process
   * terminates. Otherwise, as soon as a new minimum is reached, a new round begins. The number of samples in each
   * subsequent round is determined using the {@link Options#setResampleFactor "resample factor"}.
   * <P/>
   * At the end of the reducing process, the {@link Options#setGenDef generator definition file} for the given
   * {@link Options#setInputDef system input definition} is updated with the random seed value that produces the minimum test case count.
   */
  public void run( Options options) throws Exception
    {    
    // Identify the system input definition file.
    File inputDefOption = options.getInputDef();

    File inputDefFile;
    if( !(inputDefFile = inputDefOption).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".xml")).exists())
        {
        throw new RuntimeException( "Can't locate input file for path=" + inputDefOption);
        }

    File inputDir = inputDefFile.getParentFile();
    
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

    // Identify base test definition file.
    File baseDefFile = options.getTestDef();
    if( baseDefFile != null && !baseDefFile.isAbsolute())
      {
      // For relative path, read base test definitions from input directory.
      baseDefFile = new File( inputDir, baseDefFile.getPath());
      }

    SystemTestDef baseDef = null;
    if( baseDefFile != null && baseDefFile.exists())
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
    if( genDefFile == null)
      {
      genDefFile = new File( inputDir, Tcases.getProjectName( inputDefFile) + "-Generators.xml");
      }
    else if( !genDefFile.isAbsolute())
      {
      genDefFile = new File( inputDir, genDefFile.getPath());
      }
      
    // Previous generator definitions exist?
    IGeneratorSet genDef = null;
    if( genDefFile.exists())
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

    // Identify generators to reduce.
    String function = options.getFunction();
    ITestCaseGenerator functionGenerator = genDef.getGenerator( function);
    FunctionInputDef[] functionInputDefs;
    if( function == null)
      {
      functionInputDefs = IteratorUtils.toArray( inputDef.getFunctionInputDefs(), FunctionInputDef.class);
      }
    else if( inputDef.getFunctionInputDef( function) == null)
      {
      throw new RuntimeException( "Function=" + function + " is not defined");
      }
    else if( functionGenerator == null || functionGenerator.equals( genDef.getGenerator( null)))
      {
      throw new RuntimeException( "No generator defined for function=" + function);
      }
    else
      {
      functionInputDefs = new FunctionInputDef[]{ inputDef.getFunctionInputDef( function) };
      }

    // Find a seed that generates minimum test cases for the specified function(s).
    int initialCount = getTestCaseCount( baseDef, genDef, functionInputDefs);
    int samples;
    int round;
    int minCount;
    long minSeed;
    boolean reducing;
    Random random;
    for( samples = options.getSamples(),
           round = 1,
           minCount = initialCount,
           minSeed = 0L,
           reducing = true,
           random = new Random();
         
         samples > 0
           && reducing;
         
         samples = (int) Math.floor( samples * ( 1 + options.getResampleFactor())),
           round++)
      {
      // Perform next round of samples.
      int roundCount;
      long roundSeed;
      int i;
      for( i = 0,
             roundCount = 0,
             roundSeed = 0;
           
           i < samples
             && (roundCount =
                   getTestCaseCount
                   ( baseDef,
                     genDef,
                     functionInputDefs,
                     (roundSeed = (long) (random.nextDouble() * Long.MAX_VALUE))))
                 >= minCount;
           i++);

      reducing = i < samples;
      if( reducing)
        {
        logger_.info( "Round {}: after {} samples, reached {} test cases", new Object[]{ round, i+1, roundCount});
        minCount = roundCount;
        minSeed = roundSeed;
        }
      else
        {
        logger_.info( "Round {}: after {} samples, terminating", round, samples);
        }
      } 

    if( minCount < initialCount)
      {
      // Write updates to generator definitions.
      setRandomSeed( genDef, functionInputDefs, minSeed);
      GeneratorSetDocWriter genWriter = null;
      try
        {
        logger_.info( "Updating generator definition={}", genDefFile);
        genWriter = new GeneratorSetDocWriter( new FileOutputStream( genDefFile));
        genWriter.write( genDef);
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't write generator definition file=" + genDefFile, e);
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

  /**
   * Returns the total number of test cases generated for the given functions.
   */
  private int getTestCaseCount( SystemTestDef baseDef, IGeneratorSet genDef, FunctionInputDef[] functionInputDefs)
    {
    int testCaseCount = 0;

    for( int i = 0; i < functionInputDefs.length; i++)
      {
      FunctionInputDef functionInputDef = functionInputDefs[i];
      FunctionTestDef functionBase = baseDef==null? null : baseDef.getFunctionTestDef( functionInputDef.getName());
      TupleGenerator functionGen = (TupleGenerator) genDef.getGenerator( functionInputDef.getName());
      FunctionTestDef functionTestDef = functionGen.getTests( functionInputDef, functionBase);

      for( Iterator<TestCase> testCases = functionTestDef.getTestCases();
           testCases.hasNext();
           testCaseCount++, testCases.next());
      }

    return testCaseCount;
    }

  /**
   * Returns the total number of test cases generated for the given functions.
   */
  private int getTestCaseCount( SystemTestDef baseDef, IGeneratorSet genDef, FunctionInputDef[] functionInputDefs, long seed)
    {
    setRandomSeed( genDef, functionInputDefs, seed);
    return getTestCaseCount( baseDef, genDef, functionInputDefs);
    }

  /**
   * Returns the total number of test cases generated for the given functions.
   */
  private void setRandomSeed( IGeneratorSet genDef, FunctionInputDef[] functionInputDefs, long seed)
    {
    for( int i = 0; i < functionInputDefs.length; i++)
      {
      FunctionInputDef functionInputDef = functionInputDefs[i];
      TupleGenerator functionGen = (TupleGenerator) genDef.getGenerator( functionInputDef.getName());
      functionGen.setRandomSeed( seed);
      }
    }

  private static final Logger logger_ = LoggerFactory.getLogger( Reducer.class);
  }
