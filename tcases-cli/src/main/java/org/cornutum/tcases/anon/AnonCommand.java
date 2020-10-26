//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.anon;

import org.cornutum.tcases.HelpException;
import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.io.Resource;
import org.cornutum.tcases.io.SystemInputDocWriter;
import org.cornutum.tcases.io.SystemInputJsonWriter;
import org.cornutum.tcases.io.SystemInputResource;
import static org.cornutum.tcases.io.Resource.withDefaultType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static org.apache.commons.lang3.ObjectUtils.firstNonNull;

import java.io.File;

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
     * Throws a IllegalArgumentException reporting a missing option value
     */
    protected void throwMissingValue( String option)
      {
      throwUsageException( String.format( "No value given for %s option", option));
      }

    /**
     * Throws a IllegalArgumentException reporting a command line error.
     */
    protected void throwUsageException( String detail)
      {
      throwUsageException( detail, null);
      }

    /**
     * Throws a IllegalArgumentException reporting a command line error.
     */
    protected void throwUsageException( String detail, Exception cause)
      {
      throw
        new IllegalArgumentException
        ( "Invalid command line argument. For all command line details, use the -help option.",
          new IllegalArgumentException( detail, cause));
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

    public String toString()
      {
      StringBuilder builder = new StringBuilder();

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
    private File workingDir_;
    private boolean showVersion_;
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
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.json")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".json")).exists())
        {
        throw new RuntimeException( "Can't locate input file for path=" + options.getInputDef());
        }

    Resource.Type defaultContentType =
      firstNonNull
      ( options.getContentType(),
        Resource.Type.of( inputDefFile),
        Resource.Type.XML);

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
    Anonymizer anonymizer = new Anonymizer();
    SystemInputDef anonInputDef = anonymizer.anonymize( inputDef);

    // Write anonymized system input definition.
    Resource.Type outputFileType = firstNonNull( Resource.Type.of( withDefaultType( inputDefFile, defaultContentType)), defaultContentType);
    try
      {
      if( outputFileType == Resource.Type.JSON)
        {
        new SystemInputJsonWriter().write( anonInputDef);
        }
      else
        {
        new SystemInputDocWriter().write( anonInputDef);
        }
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write anonymized system input definition", e);
      }
    }


  /**
   * Returns a description of the current version.
   */
  public static String getVersion()
    {
    return null;
    }

  private static final Logger logger_ = LoggerFactory.getLogger( AnonCommand.class);
  }
