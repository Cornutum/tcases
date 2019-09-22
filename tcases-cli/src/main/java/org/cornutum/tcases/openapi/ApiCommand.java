//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.TcasesIO;
import org.cornutum.tcases.io.AbstractFilter;
import org.cornutum.tcases.io.TestDefToHtmlFilter;
import org.cornutum.tcases.io.TestDefToJUnitFilter;
import org.cornutum.tcases.openapi.io.TcasesOpenApiIO;
import org.cornutum.tcases.util.MapBuilder;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static org.apache.commons.io.FilenameUtils.getBaseName;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Optional;
import java.util.Properties;

/**
 * Generates input models and test models for API clients and servers, based on an OpenAPI v3 compliant API spec.
 */
public class ApiCommand
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
   * [-C | S]
   * [-I]
   * [-c {log | fail | ignore}]
   * [-f <I>outFile</I>]
   * [-o <I>outDir</I>]
   * [-R]
   * [-W]
   * [-J | -H]
   * [-T <I>contentType</I>]
   * [-v]
   * [<I>apiSpec</I>]
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
   * <NOBR>-C | -S</NOBR>
   * </TD>
   * <TD>
   * If <I>-C</I> is given, produce results to test inputs to an API client, i.e. API responses.
   * If <I>-S</I> is given, produce results to test inputs to an API server, i.e. API requests.
   * If neither is given, the default is <I>-S</I>.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-I</NOBR>
   * </TD>
   * <TD>
   * Produce an {@link SystemInputDef input definition file} for either an API client (<I>-C</I>) or an API server (<I>-P</I>).
   * If omitted, produce the corresponding {@link org.cornutum.tcases.SystemTestDef test definition file}.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>[-c {log | fail | ignore}]</NOBR>
   * </TD>
   * <TD>
   * Defines how input modelling conditions are reported. If <CODE>log</CODE> is specified, conditions are reported using log messages.
   * If <CODE>fail</CODE> is specified, any condition will cause an exception. If <CODE>ignore</CODE> is specified, all conditions
   * are silently ignored. If <I>-c</I> is omitted, the default is <CODE>log</CODE>.
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
   * If <I>-f</I> is defined, output is written to the specified <I>outFile</I>, relative to the given <I>outDir</I>.
   * If omitted, the default <I>outFile</I> is derived from the <I>apiSpec</I>.
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
   * If <I>-o</I> is defined, output is written to the specified directory.
   * If omitted, the default <I>outDir</I> is the directory containing the <I>apiSpec</I> or,
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
   * <NOBR>-R </NOBR>
   * </TD>
   * <TD>
   * If specified, tests will be generated assuming that the API will strictly enforce exclusion of "readOnly"
   * properties from request parameters. If omitted, no strict enforcement is assumed.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-W </NOBR>
   * </TD>
   * <TD>
   * If specified, tests will be generated assuming that the API will strictly enforce exclusion of "writeOnly"
   * properties from responses. If omitted, no strict enforcement is assumed.
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
   * test class. The resulting Java source file is written to the specified <I>outDir</I>.
   * Ignored if <I>-I</I> is specified.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-H </NOBR>
   * </TD>
   * <TD>
   * If <I>-H</I> is defined, test definition output is transformed into an HTML report. The resulting HTML file is written to the specified <I>outDir</I>.
   * Ignored if <I>-I</I> is specified.
   * </TD>
   * </TR>

   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-T <I>contentType</I> </NOBR>
   * </TD>
   * <TD>
   * Defines the content type of the OpenApi specification. The <I>contentType</I> must be one of "json", "yaml", or "yml".
   * The default content type is assumed for any file that is not specified explicitly or that does not have a recognized extension.
   * If omitted, the default content type is derived from the <I>apiSpec</I> name.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-v </NOBR>
   * </TD>
   * <TD>
   * Prints the current command version identifier to standard output.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR><I>apiSpec</I> </NOBR>
   * </TD>
   * <TD>
   * An OpenAPI v3 API spec is read from the given <I>apiSpec</I> file. If omitted, the API spec is
   * read from standard input. If no <I>outFile</I> is specified, output is written to a default file
   * derived from the <I>apiSpec</I> or, if no <I>apiSpec</I> is given, to standard output.
   * <P/>
   * Suppose that the base name of <I>apiSpec</I> (less any extension) is <I>B</I>. Then, assuming
   * defaults for all options, output will be a test definition for API requests written to a file
   * named "<I>B</I>-Requests-Test.json". If <I>-C</I> is specified, output will be a test
   * definition for API responses written to a file named "<I>B</I>-Responses-Test.json". If
   * <I>-I</I> is specified, output will be the corresponding input definition, written to either
   * "<I>B</I>-Requests-Input.json" or "<I>B</I>-Responses-Input.json", respectively.
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
    public enum TransformType { HTML, JUNIT };

    /**
     * Creates a new Options object.
     */
    public Options()
      {
      setWorkingDir( null);
      setServerTest( true);
      setTests( true);
      setModelOptions( new ModelOptions());
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

      else if( arg.equals( "-c"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        String notifier = args[i];
        getModelOptions().setConditionNotifier(
          Optional.ofNullable(
            "log".equals( notifier)?
            ModelConditionNotifier.log() :

            "fail".equals( notifier)?
            ModelConditionNotifier.fail() :

            "ignore".equals( notifier)?
            ModelConditionNotifier.ignore() :

            null)

          .orElseThrow( () -> getUsageException( "Unknown condition notifier: " + notifier, null)));
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

      else if( arg.equals( "-R"))
        {
        getModelOptions().setReadOnlyEnforced( true);
        }

      else if( arg.equals( "-W"))
        {
        getModelOptions().setWriteOnlyEnforced( true);
        }

      else if( arg.equals( "-T"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
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

      else if( arg.equals( "-C"))
        {
        setServerTest( false);
        }

      else if( arg.equals( "-S"))
        {
        setServerTest( true);
        }

      else if( arg.equals( "-I"))
        {
        setTests( false);
        }

      else if( arg.equals( "-J"))
        {
        if( getTransformType() != null)
          {
          throwUsageException( "Can't specify multiple output transforms");
          }
        setTransformType( TransformType.JUNIT);
        }

      else if( arg.equals( "-H"))
        {
        if( getTransformType() != null)
          {
          throwUsageException( "Can't specify multiple output transforms");
          }
        setTransformType( TransformType.HTML);
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
        setApiSpec( new File( args[i]));
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
      throw getUsageException( detail, cause);
      }

    /**
     * Returns a RuntimeException reporting a command line error.
     */
    protected RuntimeException getUsageException( String detail, Exception cause)
      {
      if( detail != null)
        {
        cause = new RuntimeException( detail, cause);
        }

      return
        new IllegalArgumentException
        ( "Usage: "
          + ApiCommand.class.getSimpleName()
          + " [-v]"
          + " [-C | -S]"
          + " [-I]"
          + " [-R]"
          + " [-W]"
          + " [-c {log | fail | ignore}]"
          + " [-f outFile]"
          + " [-o outDir]"
          + " [ -J | -H]"
          + " [-T contentType]"
          + " [apiSpec]",
          cause);
      }

    /**
     * Changes the output directory for command output.
     */
    public void setOutDir( File outDir)
      {
      outDir_ = outDir;
      }

    /**
     * Returns the output directory for command output.
     */
    public File getOutDir()
      {
      return outDir_;
      }

    /**
     * Changes the output file for command output.
     */
    public void setOutFile( File outFile)
      {
      outFile_ = outFile;
      }

    /**
     * Returns the output file for command output.
     */
    public File getOutFile()
      {
      return outFile_;
      }

    /**
     * Changes if output if for testing an API server (true) or an API client (false).
     */
    public void setServerTest( boolean serverTest)
      {
      serverTest_ = serverTest;
      }

    /**
     * Return if output if for testing an API server (true) or an API client (false).
     */
    public boolean isServerTest()
      {
      return serverTest_;
      }

    /**
     * Changes if output consists of a test definition (true) or an input definition (false).
     */
    public void setTests( boolean tests)
      {
      tests_ = tests;
      }

    /**
     * Returns if output consists of a test definition (true) or an input definition (false).
     */
    public boolean isTests()
      {
      return tests_;
      }

    /**
     * Changes the output transform type.
     */
    public void setTransformType( TransformType transformType)
      {
      transformType_ = transformType;
      }

    /**
     * Returns the output transform type.
     */
    public TransformType getTransformType()
      {
      return transformType_;
      }

    /**
     * Changes the OpenApi spec file content type.
     */
    public void setContentType( String option)
      {
      String contentType =
        Optional.ofNullable( option)
        .map( String::toLowerCase)
        .filter( type -> "json".equals( type) || "yml".equals( type) || "yaml".equals( type))
        .orElse( null);
      
      if( option != null && contentType == null)
        {
        throw new IllegalArgumentException( String.format( "'%s' is not a valid content type", option));
        }

      contentType_ = contentType;
      }

    /**
     * Returns the OpenApi spec file content type.
     */
    public String getContentType()
      {
      return contentType_;
      }

    /**
     * Changes the input modelling options.
     */
    public void setModelOptions( ModelOptions modelOptions)
      {
      modelOptions_ = modelOptions;
      }

    /**
     * Returns the input modelling options.
     */
    public ModelOptions getModelOptions()
      {
      return modelOptions_;
      }

    /**
     * Changes the Open API v3 API spec file
     */
    public void setApiSpec( File apiSpec)
      {
      apiSpec_ = apiSpec;
      }

    /**
     * Returns the Open API v3 API spec file
     */
    public File getApiSpec()
      {
      return apiSpec_;
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
     * Returns a new Options builder.
     */
    public static Builder builder()
      {
      return new Builder();
      }

    public String toString()
      {
      StringBuilder builder = new StringBuilder();

      if( isServerTest())
        {
        builder.append( " -S");
        }
      else
        {
        builder.append( " -C");
        }

      if( !isTests())
        {
        builder.append( " -I");
        }

      if( getModelOptions().isReadOnlyEnforced())
        {
        builder.append( " -R");
        }

      if( getModelOptions().isWriteOnlyEnforced())
        {
        builder.append( " -W");
        }
      
      if( getOutFile() != null)
        {
        builder.append( " -f ").append( getOutFile().getPath());
        }

      if( getOutDir() != null)
        {
        builder.append( " -o ").append( getOutDir().getPath());
        }

      if( getTransformType() == TransformType.JUNIT)
        {
        builder.append( " -J");
        }

      if( getTransformType() == TransformType.HTML)
        {
        builder.append( " -H");
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

    private File apiSpec_;
    private File outDir_;
    private File outFile_;
    private boolean serverTest_;
    private boolean tests_;
    private TransformType transformType_;
    private String contentType_;
    private ModelOptions modelOptions_;
    private File workingDir_;
    private boolean showVersion_;

    public static class Builder
      {
      public Builder()
        {
        options_ = new Options();
        }

      public Builder apiSpec( File apiSpec)
        {
        options_.setApiSpec( apiSpec);
        return this;
        }

      public Builder outDir( File outDir)
        {
        options_.setOutDir( outDir);
        return this;
        }

      public Builder outFile( File outFile)
        {
        options_.setOutFile( outFile);
        return this;
        }

      public Builder client()
        {
        options_.setServerTest( false);
        return this;
        }

      public Builder server()
        {
        options_.setServerTest( true);
        return this;
        }

      public Builder transformType( TransformType transformType)
        {
        options_.setTransformType( transformType);
        return this;
        }

      public Builder contentType( String type)
        {
        options_.setContentType( type);
        return this;
        }

      public Builder inputDef()
        {
        options_.setTests( false);
        return this;
        }

      public Builder testDef()
        {
        options_.setTests( true);
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
   * Creates a new ApiCommand object.
   */
  private ApiCommand()
    {
    // Static methods only
    }

  /**
   * Generates input models and test models for API clients and servers, based on an OpenAPI v3 compliant API spec,
   * using the given {@link Options command line options}.
   */
  public static void main( String[] args)
    {
    int exitCode = 0;
    try
      {
      run( new Options( args));
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
   * Generates input models and test models for API clients and servers, based on an OpenAPI v3 compliant API spec,
   * using the given {@link Options command line options}.
   */
  public static void run( Options options) throws Exception
    {
    if( options.showVersion())
      {
      System.out.println( getVersion());
      return;
      }
    logger_.info( "{}", getVersion());

    // Identify the API spec file
    File apiSpecFile = options.getApiSpec();
    if( apiSpecFile != null && !apiSpecFile.isAbsolute())
      {
      apiSpecFile = new File( options.getWorkingDir(), apiSpecFile.getPath());
      }
    
    // Generate requested input definition
    logger_.info( "Reading API spec from {}", apiSpecFile==null? "standard input" : apiSpecFile);
    SystemInputDef inputDef =
      options.isServerTest()
      ? TcasesOpenApiIO.getRequestInputModel( apiSpecFile, options.getContentType(), options.getModelOptions())
      : TcasesOpenApiIO.getResponseInputModel( apiSpecFile, options.getContentType(), options.getModelOptions());

    if( inputDef == null)
      {
      logger_.warn( "No {} defined", options.isServerTest()? "requests" : "responses");
      }
    else
      {
      // Output file defined?
      File outputDir = options.getOutDir();
      File outputFile = options.getOutFile();
      if( outputFile == null && apiSpecFile != null)
        {
        // No, use default name
        outputFile =
          new File(
            String.format(
              "%s%s-%s-%s.json",
              Optional.ofNullable( apiSpecFile.getParent()).map( p -> p + "/").orElse( ""),
              getBaseName( apiSpecFile.getName()),
              options.isServerTest()? "Requests" : "Responses",
              options.isTests()? "Test" : "Input"));
        }
      if( outputFile != null)
        {
        // Ensure output directory exists.
        if( outputDir == null)
          {
          File inputDir =
            apiSpecFile==null
            ? options.getWorkingDir()
            : apiSpecFile.getParentFile();

          outputDir =
            outputFile.isAbsolute()
            ? outputFile.getParentFile()
            : inputDir;
          }
        if( !outputDir.exists() && !outputDir.mkdirs())
          {
          throw new RuntimeException( "Can't create output directory=" + outputDir);
          }

        outputFile = new File( outputDir, outputFile.getName());
        }

      // Identify test definition transformations.
      AbstractFilter transformer = null;
      if( options.isTests() && options.getTransformType() != null)
        {
        if( options.getTransformType() == Options.TransformType.JUNIT)
          {
          transformer = new TestDefToJUnitFilter( MapBuilder.of( "system", (Object)inputDef.getName()).build());
          outputFile =
            Optional.ofNullable( outputFile)
            .map( f -> new File( f.getParentFile(), getBaseName( f.getName()).replaceAll( "\\W+", "") + ".java"))
            .orElse( null);
          }

        else if( options.getTransformType() == Options.TransformType.HTML)
          {
          transformer = new TestDefToHtmlFilter();
          outputFile =
            Optional.ofNullable( outputFile)
            .map( f -> new File( f.getParentFile(), getBaseName( f.getName()) + ".htm"))
            .orElse( null);
          }

        transformer.setTarget( outputFile);
        }

      OutputStream outputStream = null;
      try
        {
        outputStream =
          // Transformed output?
          transformer != null?
          transformer.getSource() :

          // Output file?
          outputFile != null?
          new FileOutputStream( outputFile) :

          // Standard output?
          null;
        }
      catch( Exception e)
        {
        throw new IllegalArgumentException( "Can't open output file=" + outputFile);
        }

      // Write requested results
      logger_.info( "Writing results to {}", outputFile==null? "standard output" : outputFile);
      if( !options.isTests())
        {
        TcasesOpenApiIO.writeInputModel( inputDef, outputStream);
        }
      else if( transformer != null)
        {
        TcasesIO.writeTests( Tcases.getTests( inputDef, null, null), outputStream);
        }
      else
        {
        TcasesOpenApiIO.writeTests( Tcases.getTests( inputDef, null, null), outputStream);
        }
      }
    }


  /**
   * Returns a description of the current version.
   */
  public static String getVersion()
    {
    Properties tcasesProperties = new Properties();
    String tcasesPropertyFileName = "/tcases.properties";
    InputStream tcasesPropertyFile = null;
    try
      {
      tcasesPropertyFile = Tcases.class.getResourceAsStream( tcasesPropertyFileName);
      tcasesProperties.load( tcasesPropertyFile);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read " + tcasesPropertyFileName, e);
      }
    finally
      {
      IOUtils.closeQuietly( tcasesPropertyFile);
      }

    return
      String.format(
        "%s (%s)",
        tcasesProperties.getProperty( "tcases.version"),
        tcasesProperties.getProperty( "tcases.date"));
    }

  private static final Logger logger_ = LoggerFactory.getLogger( ApiCommand.class);
  }
