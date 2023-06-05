//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.anon;

import org.cornutum.tcases.HelpException;
import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.TcasesIO;
import org.cornutum.tcases.TcasesJson;
import org.cornutum.tcases.generator.IGeneratorSet;
import org.cornutum.tcases.generator.io.GeneratorSetResource;
import org.cornutum.tcases.io.Resource;
import org.cornutum.tcases.io.SystemInputResource;
import static org.cornutum.tcases.CommandUtils.*;
import static org.cornutum.tcases.io.Resource.withDefaultType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static org.apache.commons.lang3.ObjectUtils.firstNonNull;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Optional;

/**
 * Converts a system input definition into an equivalent form using anonymous identifiers.
 */
public class AnonCommand
  {
  public static class Options
    {
    /**
     * Creates a new Options object.
     */
    public Options()
      {
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

      if( arg.equals( "-help"))
        {
        throwHelpException();
        }

      else if( arg.equals( "-v"))
        {
        setShowVersion( true);
        }

      else if( arg.equals( "-f"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        setOutFile( new File( args[i]));
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

      if( nargs > 1)
        {
        throwUsageException( String.format( "Unexpected argument: %s", args[i+1]));
        }

      if( nargs > 0)
        {
        setInputDef( new File( args[i]));
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
               "Usage: tcases-anon [option...] [inputDef]",
               "",
               "Prints an anonymized version of a system input definition.",
               "",
               "The system input definition is read from the given inputDef. If omitted, the system input",
               "definition is read from standard input. Otherwise, the system input definition is read from",
               "the first one of the following files that can be located.",
               "",
               "  1. inputDef",
               "  2. inputDef-Input.json",
               "  3. inputDef.json",
               "  4. inputDef-Input.xml",
               "  5. inputDef.xml",
               "",
               "Each option is one of the following:",
               "",
               "  -f outFile  If -f is defined, the anonymized system input definition is written to the given",
               "              outFile and anonymized generators are written to the same directory. If omitted,",
               "              the anonymized system input (only) is written to standard output.",
               "",
               "  -g genDef   If -g is defined, generators for the given inputDef are read from the given",
               "              genDef file. If omitted, generators are read from the corresponding *-Generators.xml",
               "              or *-Generators.json file (depending on the contentType) in the same directory as",
               "              the inputDef, if it exists.",
               "",
               "  -T contentType  Defines the default content type for the files read and produced.",
               "              The contentType must be one of 'json' or 'xml'. The default content type is",
               "              assumed for any file that is not specified explicitly or that does not have a",
               "              recognized extension. If omitted, the default content type is derived from the",
               "              inputDef name.",
               "",
               "  -l logFile  If -l is defined, log output is written to the given file. If omitted,",
               "              log output is written to a file named tcases-anon.log in the current working",
               "              directory. If logFile is 'stdout', log output is written to standard output.",
               "",
               "  -L logLevel Defines the level for log output. If omitted, the default level is INFO.",
               "              The configuration and levels used for logging are defined by the Logback system.",
               "",
               "  -v          Shows the current version. If this option is given, no other action is performed."
             })
        {
        System.err.println( line);
        }
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
     * Changes the output file for anonymized output.
     */
    public void setOutFile( File outFile)
      {
      outFile_ = outFile;
      }

    /**
     * Returns the output file for anonymized output.
     */
    public File getOutFile()
      {
      return outFile_;
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

      if( getOutFile() != null)
        {
        builder.append( " -f ").append( getOutFile().getPath());
        }

      if( getGenDef() != null)
        {
        builder.append( " -g ").append( getGenDef().getPath());
        }
      
      if( getContentType() != null)
        {
        builder.append( " -T ").append( getContentType());
        }

      if( showVersion())
        {
        builder.append( " -v");
        }

      return builder.toString();
      }

    private File inputDef_;
    private File genDef_;
    private File workingDir_;
    private boolean showVersion_;
    private File outFile_;
    private Resource.Type contentType_;

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

      public Builder genDef( File genDef)
        {
        options_.setGenDef( genDef);
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
   * Creates a new AnonCommand instance.
   */
  private AnonCommand()
    {
    // Static methods only
    }

  /**
   * Converts a system input definition into an equivalent form using anonymous identifiers.
   */
  public static void main( String[] args)
    {
    int exitCode = 0;
    try
      {
      run( new Options( args));
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
   * Converts a system input definition into an equivalent form using anonymous identifiers.
   */
  public static void run( Options options) throws Exception
    {
    if( options.showVersion())
      {
      System.out.println( getVersion());
      return;
      }
    logger_.info( "{}", getVersion());

    // Identify the system input definition file.
    File inputDefOption = options.getInputDef();
    if( inputDefOption != null && !inputDefOption.isAbsolute())
      {
      inputDefOption = new File( options.getWorkingDir(), inputDefOption.getPath());
      }

    File inputDefFile = inputDefOption;
    if( inputDefFile != null
        && !inputDefFile.exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.json")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".json")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".xml")).exists())
        {
        throw new RuntimeException( "Can't locate input file for path=" + options.getInputDef());
        }

    File inputDir =
      inputDefFile==null
      ? options.getWorkingDir()
      : inputDefFile.getParentFile();

    Resource.Type defaultContentType =
      firstNonNull
      ( options.getContentType(),
        Resource.Type.of( inputDefFile),
        Resource.Type.XML);

    // Identify the anonymized system input definition file
    File outputFile = options.getOutFile();
    if( outputFile != null)
      {
      // Ensure output directory exists.
      File outputDir = outputFile.getParentFile();
      if( outputDir != null && !outputDir.exists() && !outputDir.mkdirs())
        {
        throw new RuntimeException( "Can't create output directory=" + outputDir);
        }
      }
    Resource.Type outputFileType =
      firstNonNull(
        Resource.Type.of( outputFile),
        defaultContentType);

    // Identify the generator definition file.
    File genDefFile = options.getGenDef();
    if( genDefFile != null)
      {
      if( !genDefFile.isAbsolute())
        {
        genDefFile = new File( inputDir, genDefFile.getPath());
        }
      }
    else if( inputDefFile != null)
      {
      File genDefDefault = withDefaultType( new File( inputDir, getProjectName( inputDefFile) + "-Generators"), defaultContentType);
      if( genDefDefault.exists())
        {
        genDefFile = genDefDefault;
        }
      }
    Resource.Type genDefType =
      firstNonNull(
        Resource.Type.of( genDefFile),
        defaultContentType);

    // Identify the anonymized generator definition file
    File genOutFile =
      Optional.ofNullable( genDefFile)
      .flatMap( f -> Optional.ofNullable( outputFile))
      .map( f -> withDefaultType( new File( f.getParentFile(), getProjectName( f) + "-Generators"), outputFileType))
      .orElse( null);    

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

    // Create anonymized system input definition.
    Anonymizer anonymizer = new Anonymizer( inputDef);
    SystemInputDef anonInputDef = anonymizer.getInputDef();

    // Write anonymized system input definition.
    logger_.info( "Writing anonymized system input definition to {}", outputFile==null? "standard output" : outputFile);
    OutputStream outputStream = null;
    try
      {
      outputStream =
        outputFile != null?
        new FileOutputStream( outputFile) :
        null;
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't open output file=" + outputFile, e);
      }

    try
      {
      if( outputFileType == Resource.Type.JSON)
        {
        TcasesJson.writeInputModel( anonInputDef, outputStream);
        }
      else
        {
        TcasesIO.writeInputModel( anonInputDef, outputStream);
        }
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write anonymized system input definition", e);
      }

    if( genOutFile != null)
      {
      // Read the generator definition.
      logger_.info( "Reading generator definition={}", genDefFile);
      IGeneratorSet genDef = null;
      try( GeneratorSetResource reader = withDefaultType( GeneratorSetResource.of( genDefFile), genDefType))
        {
        genDef = reader.getGeneratorSet();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read generator definition file=" + genDefFile, e);
        }

      // Write anonymized generator definition.
      IGeneratorSet anonGenDef = anonymizer.anonymize( genDef);
      
      logger_.info( "Writing anonymized generator definition to {}", genOutFile);
      OutputStream genOutStream = null;
      try
        {
        genOutStream = new FileOutputStream( genOutFile);
        }
      catch( Exception e)
        {
        throw new IllegalStateException( "Can't open output file=" + genOutFile, e);
        }
      
      Resource.Type genOutType =
        firstNonNull(
          Resource.Type.of( genOutFile),
          outputFileType);
      try
        {
        if( genOutType == Resource.Type.JSON)
          {
          TcasesJson.writeGenerators( anonGenDef, genOutStream);
          }
        else
          {
          TcasesIO.writeGenerators( anonGenDef, genOutStream);
          }
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't write anonymized generator definition", e);
        }
      }
    }

  private static final Logger logger_ = LoggerFactory.getLogger( AnonCommand.class);
  }
