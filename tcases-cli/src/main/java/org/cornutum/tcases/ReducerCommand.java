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
import static org.cornutum.tcases.CommandUtils.*;
import static org.cornutum.tcases.io.Resource.withDefaultType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static org.apache.commons.lang3.ObjectUtils.firstNonNull;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Optional;

/**
 * For a {@link SystemInputDef system input definition}, updates the associated {@link GeneratorSet test case generators}
 * to reduce the number of generated test cases.
 *
 */
public class ReducerCommand extends Reducer
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
   * [-R]
   * [-s <I>sampleCount</I>]
   * [-t <I>baseTestDef</I>]
   * [-T <I>contentType</I>]
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
   * If <I>-g</I> is defined, update the generator specified in the given <I>genDef</I> file. Otherwise, update the default generator definition file:
   * the corresponding <NOBR><CODE>*-Generators</CODE></NOBR> file in the same directory as the <I>inputDef</I>.
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
   * <NOBR>-R </NOBR>
   * </TD>
   * <TD>
   * Ignore any random seed defined in the <I>genDef</I> file and search for a new seed to reduce test cases.
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
   * <NOBR>-T <I>contentType</I> </NOBR>
   * </TD>
   * <TD>
   * Defines the default content type for the files read and produced. The <I>contentType</I> must be one of "json" or "xml".
   * The default content type is assumed for any file that is not specified explicitly or that does not have a recognized extension.
   * If omitted, the default content type is derived from the <I>inputDef</I> name.
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
   * <LI> <I>inputDef</I>-Input.json </LI>
   * <LI> <I>inputDef</I>.json </LI>
   * </OL>
   * 
   * </TABLE>
   * </CODE>
   * </BLOCKQUOTE>
   * <P/> 
   *
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

      if( arg.equals( "-help"))
        {
        throwHelpException();
        }

      else if( arg.equals( "-f"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        setFunction( args[i]);
        }

      else if( arg.equals( "-g"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        setGenDef( new File( args[i]));
        }

      else if( arg.equals( "-r"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
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

      else if( arg.equals( "-R"))
        {
        setNewSeed( true);
        }

      else if( arg.equals( "-s"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
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
          throwMissingValue( arg);
          }
        setTestDef( new File( args[i]));
        }

      else if( arg.equals( "-T"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        try
          {
          setContentType( args[i]);
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid content type", e);
          }
        }

      else if( arg.equals( "-v"))
        {
        setShowVersion( true);
        }
      
      else
        {
        throwUsageException( String.format( "Unknown option: %s", arg));
        }
      
      return i + 1;
      }

    /**
     * Handles the non-option arguments i, i+1, ...
     */
    protected void handleArgs( String[] args, int i)
      {
      int nargs = args.length - i;
      if( nargs == 1)
        {
        setInputDef( new File( args[i]));
        }
      else if( nargs > 1)
        {
        throwUsageException( String.format( "Unexpected argument: %s", args[i+1]));
        }
      else if( !showVersion())
        {
        throwUsageException( "No input definition file specified");
        }
      }

    /**
     * Throws a HelpException after printing usage information to standard error.
     */
    protected void throwHelpException()
      {
      printUsage();
      throw new HelpException();
      }

    /**
     * Prints usage information to standard error.
     */
    protected void printUsage()
      {
      for( String line :
             new String[] {
               "Usage: tcases-reducer [option...] inputDef",
               "",
               "For a system input definition, updates the associated test case generators to reduce the number",
               "of generated test cases, using the given command line options.",
               "",
               "Each option is one of the following:",
               "",
               "  -f function        If -f is defined, update only the test case generator for the given function.",
               "                     Otherwise, update the test case generators for all functions.",
               "",
               "  -g genDef          If -g is defined, update the generator specified in the given genDef file.",
               "                     Otherwise, update the default generate definition file: the corresponding",
               "                     *-Generators.xml file in the same directory as the inputDef.",
               "",
               "  -l logFile         If -l is defined, log output is written to the given file. If omitted,",
               "                     log output is written to a file named tcases-reducer.log in the current working",
               "                     directory. If logFile is 'stdout', log output is written to standard output.",
               "",
               "  -L logLevel        Defines the level for log output. If omitted, the default level is INFO.",
               "                     The configuration and levels used for logging are defined by the Logback system.",
               "",
               "  -r resampleFactor  If -r is defined, use the given resampleFactor to determine the number",
               "                     of samples in the next round of reducing. Depending on the resampleFactor,",
               "                     the next round may use more or fewer samples. If the previous round called",
               "                     for N samples and produced a reduction, then the number of samples for the ",
               "                     next round will be N * ( 1 + resampleFactor). To increase sample count with",
               "                     each round, define resampleFactor > 0. To decrease sample count with each round,",
               "                     define -1 < resampleFactor < 0. If resampleFactor is omitted, the default value is 0.",
               "",
               "  -R                 If defined, ignore any random seed defined in the genDef file and search for a new seed.",
               "",
               "  -s sampleCount     Defines the number of samples for the initial round of reducing. If omitted,",
               "                     the default sampleCount is 10.",
               "",
               "  -t testDef         If -t is defined, generate test cases based on the test definitions in the",
               "                     specified testDef file, relative to the directory containing the inputDef.",
               "",
               "  -T contentType     Defines the default content type for the files read and produced.",
               "                     The contentType must be one of 'json' or 'xml'. The default content type is",
               "                     assumed for any file that is not specified explicitly or that does not have a",
               "                     recognized extension. If omitted, the default content type is derived from the",
               "                     inputDef name."
             })
        {
        System.err.println( line);
        }
      }

    /**
     * Changes the initial number of samples.
     */
    public void setSamples( int samples)
      {
      getReducerOptions().setSamples( samples);
      }

    /**
     * Returns the initial number of samples.
     */
    public int getSamples()
      {
      return getReducerOptions().getSamples();
      }

    /**
     * Changes the function for which tests cases are reduced.
     */
    public void setFunction( String function)
      {
      getReducerOptions().setFunction( function);
      }

    /**
     * Returns the function for which tests cases are reduced.
     */
    public String getFunction()
      {
      return getReducerOptions().getFunction();
      }

    /**
     * Changes the base test definition file.
     */
    public void setTestDef( File testDef)
      {
      testDef_ = testDef;
      }

    /**
     * Returns the base test definition file.
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
      getReducerOptions().setResampleFactor( resampleFactor);
      }

    /**
     * Returns the {@link #setResampleFactor resample factor}.
     */
    public double getResampleFactor()
      {
      return getReducerOptions().getResampleFactor();
      }

    /**
     * Changes if ignoring current random seed used by generators.
     */
    public void setNewSeed( boolean newSeed)
      {
      getReducerOptions().setNewSeed( newSeed);
      }

    /**
     * Returns if ignoring current random seed used by generators.
     */
    public boolean isNewSeed()
      {
      return getReducerOptions().isNewSeed();
      }

    /**
     * Returns the {@link ReducerOptions}
     */
    public ReducerOptions getReducerOptions()
      {
      return reducerOptions_;
      }

    /**
     * Changes the default file content type.
     */
    public void setContentType( String option)
      {
      Resource.Type contentType = Resource.Type.of( option);
      if( option != null && contentType == null)
        {
        throw new IllegalArgumentException( String.format( "'%s' is not a valid content type", option));
        }
      setContentType( contentType);
      }

    /**
     * Changes the default file content type.
     */
    public void setContentType( Resource.Type contentType)
      {
      contentType_ = contentType;
      }

    /**
     * Returns the default file content type.
     */
    public Resource.Type getContentType()
      {
      return contentType_;
      }

    /**
     * Changes if the current version should be shown.
     */
    public void setShowVersion( boolean showVersion)
      {
      showVersion_ = showVersion;
      }

    /**
     * Returns if the current version should be shown.
     */
    public boolean showVersion()
      {
      return showVersion_;
      }

    /**
     * Returns a new Options builder.
     */
    public static Builder builder()
      {
      return new Builder();
      }

    @Override
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

      if( isNewSeed())
        {
        builder.append( " -R");
        }

      if( getTestDef() != null)
        {
        builder.append( " -t ").append( getTestDef().getPath());
        }

      if( getContentType() != null)
        {
        builder.append( " -T ").append( getContentType());
        }
        
      return builder.toString();
      }

    private File inputDef_;
    private File testDef_;
    private File genDef_;
    private ReducerOptions reducerOptions_ = new ReducerOptions();
    private Resource.Type contentType_;
    private boolean showVersion_;

    public static class Builder
      {
      public Builder()
        {
        options_ = new Options();
        }

      public Builder inputDef( File inputDef)
        {
        options_.setInputDef( inputDef);
        return this;
        }

      public Builder testDef( File testDef)
        {
        options_.setTestDef( testDef);
        return this;
        }

      public Builder genDef( File genDef)
        {
        options_.setGenDef( genDef);
        return this;
        }

      public Builder function( String function)
        {
        options_.setFunction( function);
        return this;
        }

      public Builder resampleFactor( double resampleFactor)
        {
        options_.setResampleFactor( resampleFactor);
        return this;
        }

      public Builder newSeed()
        {
        options_.setNewSeed( true);
        return this;
        }

      public Builder samples( int samples)
        {
        options_.setSamples( samples);
        return this;
        }

      public Builder contentType( String type)
        {
        options_.setContentType( type);
        return this;
        }

      public Options build()
        {
        return options_;
        }
      
      private Options options_;
      }
    }
  
  /**
   * Creates a new ReducerCommand object.
   */
  public ReducerCommand()
    {
    }

  /**
   * For a {@link SystemInputDef system input definition}, updates the associated {@link GeneratorSet test case generators}
   * to reduce the number of generated test cases, using the given {@link Options command line options}.
   * <P/>
   * @see ReducerCommand#run
   */
  public static void main( String[] args)
    {
    int exitCode = 0;
    try
      {
      ReducerCommand reducer = new ReducerCommand();
      reducer.run( new Options( args));
      }
    catch( HelpException h)
      {
      exitCode = 1;
      }
    catch( Throwable e)
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
    if( options.showVersion())
      {
      System.out.println( getVersion());
      return;
      }
    logger_.info( "{}", getVersion());

    // Identify the system input definition file.
    File inputDefOption = options.getInputDef();

    File inputDefFile;
    if( !(inputDefFile = inputDefOption).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.json")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".json")).exists())
        {
        throw new RuntimeException( "Can't locate input file for path=" + inputDefOption);
        };

    Resource.Type defaultContentType =
      firstNonNull
      ( options.getContentType(),
        Resource.Type.of( inputDefFile),
        Resource.Type.XML);

    File inputDir = inputDefFile.getParentFile();
    String project = getProjectName( inputDefFile);
    
    // Read the system input definition.
    logger_.info( "Reading system input definition={}", inputDefFile);
    SystemInputDef inputDef = null;
    try( SystemInputResource reader = withDefaultType( SystemInputResource.of( inputDefFile), defaultContentType))
      {
      inputDef = reader.getSystemInputDef();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read input definition file=" + inputDefFile, e);
      }

    // Identify base test definition file.
    File baseDefFile = options.getTestDef();
    if( baseDefFile != null && !baseDefFile.isAbsolute())
      {
      // For relative path, read base test definitions from input directory.
      baseDefFile = new File( inputDir, baseDefFile.getPath());
      }

    SystemTestDef baseDef = null;
    if( baseDefFile != null)
      {
      // Read the previous base test definitions.
      logger_.info( "Reading base test definition={}", baseDefFile);
      try( SystemTestResource reader = withDefaultType( SystemTestResource.of( baseDefFile), defaultContentType))
        {
        baseDef = reader.getSystemTestDef();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read test definition file=" + baseDefFile, e);
        }
      }

    // Identify the generator definition file.
    File genDefFile = options.getGenDef();
    if( genDefFile == null)
      {
      genDefFile = withDefaultType( new File( inputDir, project + "-Generators"), defaultContentType);
      }
    else if( !genDefFile.isAbsolute())
      {
      genDefFile = new File( inputDir, genDefFile.getPath());
      }
      
    // Previous generator definitions exist?
    GeneratorSet genDef = null;
    if( genDefFile.exists())
      {
      // Yes, read generator definitions.
      logger_.info( "Reading generator definition={}", genDefFile);
      try( GeneratorSetResource reader = withDefaultType( GeneratorSetResource.of( genDefFile), defaultContentType))
        {
        genDef = (GeneratorSet) reader.getGeneratorSet();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read generator definition file=" + genDefFile, e);
        }
      }
    else
      {
      // No, use default TupleGenerator.
      GeneratorSet genSet = new GeneratorSet();
      genSet.addGenerator( GeneratorSet.ALL, new TupleGenerator());
      genDef = genSet;
      } 

    // Get generators that reduce test cases for the specified function(s)
    Optional<GeneratorSet> genDefNew = reduce( inputDef, genDef, baseDef, options.getReducerOptions());
    if( !genDefNew.isPresent())
      {
      logger_.info( "[{}] Generator definition not changed", project);
      }
    else
      {
      try
        {
        Resource.Type genDefType = firstNonNull( Resource.Type.of( withDefaultType( genDefFile, defaultContentType)), defaultContentType);
        if( genDefType == Resource.Type.JSON)
          {
          TcasesJson.writeGenerators( genDefNew.get(), new FileOutputStream( genDefFile));
          }
        else
          {
          TcasesIO.writeGenerators( genDefNew.get(), new FileOutputStream( genDefFile));
          }
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't write generator definition file=" + genDefFile, e);
        }      
      }
    }

  private static final Logger logger_ = LoggerFactory.getLogger( ReducerCommand.class);
  }
