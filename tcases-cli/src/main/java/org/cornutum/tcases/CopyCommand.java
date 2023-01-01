//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
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
import java.io.File;
import java.util.Optional;

/**
 * Copies a Tcases project
 */
public class CopyCommand
  {
  /**
   * Represents a set of command line options.
   */
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

      else if( arg.equals( "-t"))
          {
          i++;
          if( i >= args.length)
            {
            throwMissingValue( arg);
            }
          setTestDef( new File( args[i]));
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
          setContentType( args[i]);
          }

        else if( arg.equals( "--toDir"))
          {
          i++;
          if( i >= args.length)
            {
            throwMissingValue( arg);
            }
          setDestDir( new File( args[i]));
          }

        else if( arg.equals( "--toName"))
          {
          i++;
          if( i >= args.length)
            {
            throwMissingValue( arg);
            }
          setDestName( args[i]);
          }

        else if( arg.equals( "--toType"))
          {
          i++;
          if( i >= args.length)
            {
            throwMissingValue( arg);
            }
          setDestType( args[i]);
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

      if( nargs < 1)
        {
        throwUsageException( "No project inputDef specified");
        }

      setInputDef( new File( args[i]));
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
               "Usage: tcases-copy [options...] inputDef",
               "",
               "Copies a Tcases project, according to the given command.",
               "",
               "The project is identified by given system input definition file. The system input",
               "definition is read from the first one of the following files that can be located.",
               "",
               "  1. inputDef",
               "  2. inputDef-Input.json",
               "  3. inputDef.json",
               "  4. inputDef-Input.xml",
               "  5. inputDef.xml",
               "",
               "Each option is one of the following:",
               "",
               "  -g genDef      If defined, the project generator definition is read from the given ",
               "                 genDef file. Otherwise, the generator definition is read from the",
               "                 standard location for the project.",
               "",
               "  -t testDef     If defined, base test definitions for the project are read from the ",
               "                 given testDef file. Otherwise, base test definitions are read from",
               "                 the standard location for the project.",
               "",
               "  -T type        If defined, specifies the content type for the inputDef. The contentType",
               "                 must be one of 'json' or 'xml'. Otherwise, the content type is derived",
               "                 from the inputDef name.",
               "",
               "  --toDir outDir If defined, copied project files are written to the given directory.",
               "                 Otherwise, copied project files are written to the parent directory of",
               "                 the inputDef.",
               "",
               "  --toType type  If defined, the copied files are written using the given content type.",
               "                 Otherwise, copied files use the same content type as the inputDef.",
               "",
               "  --toName name  If defined, the copied files are written with the given project name.",
               "                 Otherwise, copied files use the same project name as the inputDef.",
               "",
               "  -l logFile     If -l is defined, log output is written to the given file. If omitted,",
               "                 log output is written to a file named tcases-copy.log in the current",
               "                 working directory. If logFile is 'stdout', log output is written to",
               "                 standard output.",
               "",
               "  -v             Shows the current Tcases version. If this option is given, no other",
               "                 action is performed."
             })
        {
        System.err.println( line);
        }
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
     * Changes the content type.
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
     * Changes the content type.
     */
    public void setContentType( Resource.Type contentType)
      {
      contentType_ = contentType;
      }

    /**
     * Returns the content type.
     */
    public Resource.Type getContentType()
      {
      return contentType_;
      }

    /**
     * Changes the destination directory.
     */
    public void setDestDir( File outDir)
      {
      destDir_ = outDir;
      }

    /**
     * Returns the destination directory.
     */
    public File getDestDir()
      {
      return destDir_;
      }

    /**
     * Changes the destination project name.
     */
    public void setDestName( String projectName)
      {
      destName_ = projectName;
      }

    /**
     * Returns the destination project name.
     */
    public String getDestName()
      {
      return destName_;
      }

    /**
     * Changes the destination content type.
     */
    public void setDestType( String option)
      {
      Resource.Type destType = Resource.Type.of( option);
      if( option != null && destType == null)
        {
        throw new IllegalArgumentException( String.format( "'%s' is not a valid content type", option));
        }
      setDestType( destType);
      }

    /**
     * Changes the destination content type.
     */
    public void setDestType( Resource.Type destType)
      {
      destType_ = destType;
      }

    /**
     * Returns the destination content type.
     */
    public Resource.Type getDestType()
      {
      return destType_;
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

      if( getContentType() != null)
        {
        builder.append( " ").append( getContentType());
        }

      if( getGenDef() != null)
        {
        builder.append( " -g ").append( getGenDef().getPath());
        }

      if( getTestDef() != null)
        {
        builder.append( " -t ").append( getTestDef().getPath());
        }

      if( getDestName() != null)
        {
        builder.append( " --toName ").append( getDestName());
        }

      if( getDestDir() != null)
        {
        builder.append( " --toDir ").append( getDestDir().getPath());
        }

      if( getDestType() != null)
        {
        builder.append( " --toType ").append( getDestType());
        }

      if( showVersion())
        {
        builder.append( " -v");
        }

      if( getInputDef() != null)
        {
        builder.append( " ").append( getInputDef().getPath());
        }

      return builder.toString();
      }

    private File inputDef_;
    private Resource.Type contentType_;
    private File destDir_;
    private File testDef_;
    private File genDef_;
    private String destName_;
    private Resource.Type destType_;
    private File workingDir_;
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

      public Builder contentType( String type)
        {
        options_.setContentType( type);
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

      public Builder destDir( File outDir)
        {
        options_.setDestDir( outDir);
        return this;
        }

      public Builder destName( String projectName)
        {
        options_.setDestName( projectName);
        return this;
        }

      public Builder destType( String type)
        {
        options_.setDestType( type);
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
   * Creates a new CopyCommand object.
   */
  private CopyCommand()
    {
    // Static methods only
    }

  /**
   * Copies a Tcases project, using the given {@link Options command line options}.
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
   * Copies a Tcases project, using the given {@link Options command line options}.
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
    File inputDefOption =
      Optional.ofNullable( options.getInputDef())
      .orElseThrow( () -> new IllegalArgumentException( "No system input definition file specified"));

    if( !inputDefOption.isAbsolute())
      {
      inputDefOption = new File( options.getWorkingDir(), inputDefOption.getPath());
      }

    File inputDefFile = inputDefOption;
    if( !inputDefFile.exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.json")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".json")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + "-Input.xml")).exists()
        && !(inputDefFile = new File( inputDefOption.getPath() + ".xml")).exists())
        {
        throw new IllegalArgumentException( "Can't locate system input definition input file=" + options.getInputDef());
        }

    // Identify the input project.
    String inputProject = getProjectName( inputDefFile);

    File inputDir = inputDefFile.getParentFile();

    Resource.Type inputContentType =
      Optional.ofNullable( options.getContentType())
      .orElse(
        Optional.ofNullable( Resource.Type.of( inputDefFile))
        .orElse( Resource.Type.JSON));
    
    // Identify the generator definition file.
    File genDefFile =
      Optional.ofNullable( options.getGenDef())
      .orElseGet( () -> withDefaultType( new File( inputDir, inputProject + "-Generators"), inputContentType));

    if( !genDefFile.isAbsolute())
      {
      // For relative path, read generator definitions from input directory.
      genDefFile = new File( inputDir, genDefFile.getPath());
      }

    // Identify the base test definition file.
    File baseDefFile =
      Optional.ofNullable( options.getTestDef())
      .orElseGet( () -> withDefaultType( new File( inputProject + "-Test"), inputContentType));
    
    if( !baseDefFile.isAbsolute())
      {
      // For relative path, read base test definitions from input directory.
      baseDefFile = new File( inputDir, baseDefFile.getPath());
      }

    // Identify the destination project.
    String destProject =
      Optional.ofNullable( options.getDestName())
      .orElse( inputProject);
    
    File destDir =
      Optional.ofNullable( options.getDestDir())
      .map( outDir -> outDir.isAbsolute()? outDir : new File( inputDir, outDir.getPath()))
      .orElse( inputDir);

    Resource.Type destContentType =
      Optional.ofNullable( options.getDestType())
      .orElse( inputContentType);

    if( destProject.equals( inputProject)
        && destDir.equals( inputDir)
        && destContentType.equals( inputContentType))
      {
      logger_.info( "No copy made -- the destination project is the same as the input project");
      }
    else
      {
      if( !destDir.exists() && !destDir.mkdirs())
        {
        throw new RuntimeException( "Can't create destination directory=" + destDir);
        }
      
      logger_.info( "Reading system input definition from {}", inputDefFile.getPath());
      SystemInputDef inputDef = null;
      try( SystemInputResource reader = withDefaultType( SystemInputResource.of( inputDefFile), inputContentType))
        {
        inputDef = reader.getSystemInputDef();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read input definition file=" + inputDefFile, e);
        }

      File inputDefCopy = withDefaultType( new File( destDir, String.format( "%s-Input", destProject)), destContentType);
      logger_.info( "Copying system input definition to {}", inputDefCopy.getPath());
      try( SystemInputResource writer = SystemInputResource.of( inputDefCopy))
        {
        writer.write( inputDef);
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't write input definition file=" + inputDefCopy, e);
        }

      if( genDefFile.exists())
        {
        logger_.info( "Reading generator definition from {}", genDefFile.getPath());
        IGeneratorSet genDef = null;
        try( GeneratorSetResource reader = withDefaultType( GeneratorSetResource.of( genDefFile), inputContentType))
          {
          genDef = reader.getGeneratorSet();
          }
        catch( Exception e)
          {
          throw new RuntimeException( "Can't read generator definition file=" + genDefFile, e);
          }

        File genDefCopy = withDefaultType( new File( destDir, String.format( "%s-Generators", destProject)), destContentType);
        logger_.info( "Copying generator definition to {}", genDefCopy.getPath());
        try( GeneratorSetResource writer = GeneratorSetResource.of( genDefCopy))
          {
          writer.write( genDef);
          }
        catch( Exception e)
          {
          throw new RuntimeException( "Can't write generator definition file=" + genDefCopy, e);
          }
        }

      if( baseDefFile.exists())
        {
        logger_.info( "Reading base test definitions from {}", baseDefFile.getPath());
        SystemTestDef testDef = null;
        try( SystemTestResource reader = withDefaultType( SystemTestResource.of( baseDefFile), inputContentType))
          {
          testDef = reader.getSystemTestDef();
          }
        catch( Exception e)
          {
          throw new RuntimeException( "Can't read test definition file=" + baseDefFile, e);
          }

        File baseDefCopy = withDefaultType( new File( destDir, String.format( "%s-Test", destProject)), destContentType);
        logger_.info( "Copying base test definitions to {}", baseDefCopy.getPath());
        try( SystemTestResource writer = SystemTestResource.of( baseDefCopy))
          {
          writer.write( testDef);
          }
        catch( Exception e)
          {
          throw new RuntimeException( "Can't write test definition file=" + baseDefCopy, e);
          }
        }
      }
    }


  private static final Logger logger_ = LoggerFactory.getLogger( Tcases.class);
  }
