//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.HelpException;
import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.TcasesIO;
import org.cornutum.tcases.io.AbstractFilter;
import org.cornutum.tcases.io.TestDefToHtmlFilter;
import org.cornutum.tcases.io.TestDefToJUnitFilter;
import org.cornutum.tcases.io.TransformFilter;
import org.cornutum.tcases.openapi.io.TcasesOpenApiIO;
import org.cornutum.tcases.openapi.resolver.ResolverConditionNotifier;
import org.cornutum.tcases.openapi.resolver.ResolverContext;
import org.cornutum.tcases.util.MapBuilder;
import static org.cornutum.tcases.CommandUtils.*;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static org.apache.commons.io.FilenameUtils.getBaseName;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Random;

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
   * [<I>option</I>...] [<I>apiSpec</I>]
   * </NOBR>
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD colspan="3">
   * where each <I>option</I> is one of the following:
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-C | -S | -D</NOBR>
   * </TD>
   * <TD>
   * If <I>-C</I> is given, produce test models for an API client, i.e. API response tests.
   * If <I>-S</I> is given, produce test models for an API server, i.e. API request tests.
   * If <I>-D</I> is given, produce request test cases for an API server, i.e. API request tests.
   * If none of these is given, the default is <I>-S</I>.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-X</NOBR>
   * </TD>
   * <TD>
   * If specified, test models are generated based on the examples specified in the <I>apiSpec</I>.
   * Otherwise, by default, test models are created by generating random input values.
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
   * Produce an {@link SystemInputDef input definition file} for either an API client (<I>-C</I>) or an API server (<I>-S</I>).
   * If omitted, produce either the corresponding {@link org.cornutum.tcases.SystemTestDef test definition file}
   * or list of {@link org.cornutum.tcases.openapi.resolver.RequestCase request test cases} (<I>-D</I>).
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-c M[,R] </NOBR>
   * </TD>
   * <TD>
   * Defines how input modelling and request case resolution conditions are reported. Both <CODE>M</CODE> (for modelling conditions) and <CODE>R</CODE> (for
   * resolution conditions) must be one of <CODE>log</CODE>, <CODE>fail</CODE>, or <CODE>ignore</CODE>.
   * If <CODE>log</CODE> is specified, conditions are reported using log messages.
   * If <CODE>fail</CODE> is specified, any condition will cause an exception. If <CODE>ignore</CODE> is specified, all conditions
   * are silently ignored. If <CODE>R</CODE> is omitted, the default is <CODE>log</CODE>. If <I>-c</I> is omitted, the default is <CODE>log,log</CODE>.

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
   * directory containing the <I>apiSpec</I>.
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
   * When <I>-D</I> is specified, use the given random number seed to generate request test case input values. 
   * If omitted, the default random number seed is derived from the <I>apiSpec</I> name.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-m <I>maxTries</I> </NOBR>
   * </TD>
   * <TD>
   * When <I>-D</I> is specified, defines the maximum attempts made to resolve a request test case input value before reporting failure.
   * If omitted, the default value is 10000.
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
   * Defines the content type of the OpenApi specification. The <I>contentType</I> must be one of "json", "yaml", or "yml".
   * If omitted, the default content type is derived from the <I>apiSpec</I> name. If the <I>apiSpec</I> is read from standard
   * input or does not have a recognized extension, the default content type is "json".
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
   * definition for API responses written to a file named "<I>B</I>-Responses-Test.json".
   * If <I>-D</I> is specified, output will be a list of request test cases written to a file named "<I>B</I>-Request-Cases.json".
   * If <I>-I</I> is specified, output will be the corresponding input definition, written to either
   * "<I>B</I>-Requests-Input.json" or "<I>B</I>-Responses-Input.json", respectively.
   * </TD>
   * </TR>
   *
   * </TABLE>
   * </CODE>
   * </BLOCKQUOTE>
   */
  public static class Options
    {
    public enum TransformType { HTML, JUNIT, CUSTOM };

    /**
     * Creates a new Options object.
     */
    public Options()
      {
      setWorkingDir( null);
      setServerTest( true);
      setTests( true);
      setModelOptions( new ModelOptions());
      setResolverContext( new ResolverContext( new Random()));
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

      else if( arg.equals( "-o"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        setOutDir( new File( args[i]));
        }

      else if( arg.equals( "-c"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        setConditionNotifiers( args[i]);
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

      else if( arg.equals( "-R"))
        {
        setReadOnlyEnforced( true);
        }

      else if( arg.equals( "-W"))
        {
        setWriteOnlyEnforced( true);
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

      else if( arg.equals( "-C"))
        {
        setServerTest( false);
        }

      else if( arg.equals( "-S"))
        {
        setServerTest( true);
        }

      else if( arg.equals( "-D"))
        {
        setRequestCases( true);
        }

      else if( arg.equals( "-X"))
        {
        setSource( ModelOptions.Source.EXAMPLES);
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

      else if( arg.equals( "-x"))
        {
        if( getTransformType() != null)
          {
          throwUsageException( "Can't specify multiple output transforms");
          }
        setTransformType( TransformType.CUSTOM);

        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        setTransformDef( new File( args[i]));
        }

      else if( arg.equals( "-p"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
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

      else if( arg.equals( "-r"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
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

      else if( arg.equals( "-m"))
        {
        i++;
        if( i >= args.length)
          {
          throwMissingValue( arg);
          }
        try
          {
          setMaxTries( Integer.parseInt( args[i]));
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid max tries", e);
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
        setApiSpec( new File( args[i]));
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
               "Usage: tcases-api [option...] [apiSpec]",
               "",
               "Generates input models and test models for API clients and servers, based on an OpenAPI v3 compliant API spec.",
               "",
               "An OpenApi v3 API spec is read from the given apiSpec file. If omitted, the API spec is read from standard input.",
               "If no outFile is specified, output is written to a default file derived from the apiSpec or, if no apiSpec is",
               "given, to standard output.",
               "",
               "Suppose that the base name of apiSpec (less any extension) is B. Then, assuming defaults for all options, output",
               "will be a test definition for API requests written to a file named 'B-Requests-Test.json'. If -C is specified,",
               "output will be a test definition for API responses written to a file named 'B-Responses-Test.json'. If -D is",
               "specified, output will be a list of request test cases written to a file named 'B-Request-Cases.json'. If -I is",
               "specified, output will be the corresponding input definition, written to either 'B-Requests-Input.json' or",
               "'B-Responses-Input.json', respectively.",
               "",
               "Each option is one of the following:",
               "",
               "  -C, -S, -D  If -C is given, produce results to test inputs to an API client, i.e. API responses. If -S is given,",
               "              produce results to test inputs to an API server, i.e. API requests. If -D is given, produce request",
               "              test cases for an API server, i.e. API request tests. If none of these is given, the default is -S.",
               "",
               "  -X          If specified, test models are generated based on the examples specified in the apiSpec. Otherwise,",
               "              by default, test models are created by generating random input values.",
               "",
               "  -I          Produce an input definition file for either an API client (-C) or an API server (-S). If omitted,",
               "              produce the corresponding test definition file.",
               "",
               "  -R          If specified, tests will be generated assuming that the API will strictly enforce exclusion of 'readOnly'",
               "              properties from request parameters. If omitted, no strict enforcement is assumed.",
               "",
               "  -W          If specified, tests will be generated assuming that the API will strictly enforce exclusion of 'writeOnly'",
               "              properties from responses. If omitted, no strict enforcement is assumed.",
               "",
               "  -c M[R]     Defines how input modelling and request case resolution conditions are reported. Both M (for modelling conditions)",
               "              and R (for resolution conditions) must be one of 'log', 'fail', or 'ignore'. If 'log' is specified, conditions are",
               "              reported using log messages. If 'fail' is specified, any condition will cause an exception. If 'ignore' is specified,",
               "              all conditions are silently ignored. If R is omitted, the default is 'log'. If -c is omitted, the default is",
               "              'log,log'.",
               "",
               "  -f outFile  If -f is defined, output is written to the specified outFile, relative to the given outDir.",
               "              If omitted, the default outFile is derived from the apiSpec.",
               "",
               "  -o outDir   If -o is defined, output is written to the specified directory. If omitted, the default outDir is",
               "              the directory containing the apiSpec or, if reading from standard input, the current working directory.",
               "              If an output path cannot be derived, output is written to standard output.",
               "",
               "  -J          If -J is defined, test definition output is transformed into Java source",
               "              code for a JUnit test class. The resulting Java source file is written to",
               "              the specified outDir.",
               "",
               "  -H          If -H is defined, test definition output is transformed into an HTML report",
               "              that is written to the specified outDir.",
               "",
               "  -x xsltDef  If -x is defined, test definition output is transformed according to the",
               "              XSLT transform defined by the xsltDef file. If relative, the xsltDef path is",
               "              assumed to be relative to the directory containing the apiSpec.",
               "",
               "  -p name=value  Defines the value of a transform parameter. Any number of -p options",
               "              may be specified. This option is meaningful only if the -x or -J option is given.",
               "",
               "  -r seed     When -D is specified, use the given random number seed to generate request test case",
               "              input values. If omitted, the default random number seed is derived from the 'apiSpec' name.",
               "",
               "  -m maxTries When -D is specified, defines the maximum attempts made to resolve a request test case input",
               "              value before reporting failure. If omitted, the default value is 10000.",
               "",
               "  -T docType  Defines the content type of the OpenApi specification. The 'docType' must be one of 'json', 'yaml',",
               "              or 'yml'. If omitted, the default content type is derived from the 'apiSpec' name. ",
               "              If the 'apiSpec' is read from standard input or does not have a recognized extension, the default",
               "              content type is 'json'.",
               "",
               "  -l logFile  If -l is defined, log output is written to the given file. If omitted,",
               "              log output is written to a file named tcases-api.log in the current working",
               "              directory. If logFile is 'stdout', log output is written to standard output.",
               "",
               "  -L logLevel Defines the level for log output. If omitted, the default level is INFO.",
               "              The configuration and levels used for logging are defined by the Logback system.",
               "",
               "  -v          Prints the current command version identifier to standard output."
             })
        {
        System.err.println( line);
        }
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
      requestCases_ = false;
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
      if( !tests)
        {
        requestCases_ = false;
        }
      }

    /**
     * Returns if output consists of a test definition (true) or an input definition (false).
     */
    public boolean isTests()
      {
      return tests_;
      }

    /**
     * Changes if test case output consists of a list of request test cases (true) or a test definition (false).
     */
    public void setRequestCases( boolean requestCases)
      {
      requestCases_ = requestCases;
      if( requestCases)
        {
        serverTest_ = true;
        }
      }

    /**
     * Returns if test case output consists of a list of request test cases (true) or a test definition (false).
     */
    public boolean isRequestCases()
      {
      return requestCases_;
      }

    /**
     * Changes the source of API input definitions.
     */
    public void setSource( ModelOptions.Source source)
      {
      getModelOptions().setSource( source);
      }

    /**
     * Changes the source of API input definitions.
     */
    public void setSource( String source)
      {
      setSource( ModelOptions.Source.valueOf( String.valueOf( source).toUpperCase()));
      }

    /**
     * Returns the source of API input definitions.
     */
    public ModelOptions.Source getSource()
      {
      return getModelOptions().getSource();
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
     * Changes the request case resolution options.
     */
    public void setResolverContext( ResolverContext resolverContext)
      {
      resolverContext_ = resolverContext;
      }

    /**
     * Returns the request case resolution options.
     */
    public ResolverContext getResolverContext()
      {
      return resolverContext_;
      }

    /**
     * Changes the random number generator seed for request case resolution.
     */
    public void setRandomSeed( Long seed)
      {
      randomSeed_ = seed;
      if( seed != null)
        {
        getResolverContext().setRandom( new Random( seed));
        }
      }

    /**
     * Returns the random number generator seed for request case resolution.
     */
    public Long getRandomSeed()
      {
      return randomSeed_;
      }

    /**
     * Returns the default random number generator seed for request case resolution.
     */
    public Long getDefaultRandomSeed()
      {
      return
        Optional.ofNullable( getApiSpec())
        .map( spec -> (long) spec.getName().hashCode())
        .orElse( new Random().nextLong());
      }

    /**
     * Changes the maximum attempts made to resolve a request test case input value before reporting failure..
     */
    public void setMaxTries( int maxTries)
      {
      getResolverContext().setMaxTries( maxTries);
      }

    /**
     * Returns the maximum attempts made to resolve a request test case input value before reporting failure..
     */
    public int getMaxTries()
      {
      return getResolverContext().getMaxTries();
      }

    /**
     * Changes condition notifiers for input modelling and request case resolution conditions.
     */
    public void setConditionNotifiers( String notifierList)
      {
      String[] notifiers = notifierList.split( ",", -1);

      String modelNotifier = notifiers.length > 0? StringUtils.trimToNull( notifiers[0]) : null;
      setOnModellingCondition( modelNotifier);

      String resolveNotifier = notifiers.length > 1? StringUtils.trimToNull( notifiers[1]) : null;
      setOnResolverCondition( resolveNotifier);
      }

    /**
     * Changes the input modelling condition notifier.
     */
    public void setOnModellingCondition( String notifier)
      {
      getModelOptions().setConditionNotifier(
        Optional.ofNullable(
          notifier == null || "log".equals( notifier)?
          ModelConditionNotifier.log() :

          "fail".equals( notifier)?
          ModelConditionNotifier.fail() :

          "ignore".equals( notifier)?
          ModelConditionNotifier.ignore() :

          null)

        .orElseThrow( () -> getUsageException( "Unknown condition notifier: " + notifier, null)));
      }

    /**
     * Changes the request case resolution condition notifier.
     */
    public void setOnResolverCondition( String notifier)
      {
      getResolverContext().setNotifier(
        Optional.ofNullable(
          notifier == null || "log".equals( notifier)?
          ResolverConditionNotifier.log() :

          "fail".equals( notifier)?
          ResolverConditionNotifier.fail() :

          "ignore".equals( notifier)?
          ResolverConditionNotifier.ignore() :

          null)

        .orElseThrow( () -> getUsageException( "Unknown condition notifier: " + notifier, null)));
      }

    /**
     * Changes the input modelling condition notifier.
     *
     * @deprecated Replace using {@link #setOnModellingCondition setOnModellingCondition()}.
     */
    @Deprecated
    public void setOnCondition( String notifier)
      {
      setOnModellingCondition( notifier);
      }

    /**
     * Changes if "readOnly" properties are strictly enforced.
     */
    public void setReadOnlyEnforced( boolean enforced)
      {
      getModelOptions().setReadOnlyEnforced( enforced);
      }

    /**
     * Changes if "writeOnly" properties are strictly enforced.
     */
    public void setWriteOnlyEnforced( boolean enforced)
      {
      getModelOptions().setWriteOnlyEnforced( enforced);
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

      if( isRequestCases())
        {
        builder.append( " -D");
        }
      else if( isServerTest())
        {
        builder.append( " -S");
        }
      else
        {
        builder.append( " -C");
        }

      if( ModelOptions.Source.EXAMPLES.equals( getSource()))
        {
        builder.append( " -X");
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

      if( getTransformDef() != null)
        {
        builder.append( " -x ").append( getTransformDef().getPath());
        }

      if( getTransformParams() != null)
        {
        for( String name : getTransformParams().keySet())
          {
          builder.append( " -p ").append( name).append( '=').append( getTransformParams().get( name));
          }
        }

      if( isRequestCases())
        {
        builder.append( " -m ").append( getMaxTries());

        if( getRandomSeed() != null)
          {
          builder.append( " -r ").append( getRandomSeed());
          }
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
    private boolean requestCases_;
    private TransformType transformType_;
    private File transformDef_;
    private Map<String,Object> transformParams_ = new HashMap<String,Object>();
    private String contentType_;
    private ModelOptions modelOptions_;
    private ResolverContext resolverContext_;
    private File workingDir_;
    private boolean showVersion_;
    private Long randomSeed_;

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

      public Builder requestCases()
        {
        options_.setRequestCases( true);
        return this;
        }

      public Builder source( ModelOptions.Source source)
        {
        options_.setSource( source);
        return this;
        }

      public Builder transformType( TransformType transformType)
        {
        options_.setTransformType( transformType);
        return this;
        }

      public Builder transformDef( File transformDef)
        {
        options_.setTransformDef( transformDef);
        return this;
        }

      public Builder transformParam( String name, String value)
        {
        options_.getTransformParams().put( name, value);
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

      /**
       * @deprecated Replace with {@link #onModellingCondition onModellingCondition()}
       */
      @Deprecated
      public Builder onCondition( String notifier)
        {
        return onModellingCondition( notifier);
        }

      public Builder onModellingCondition( String notifier)
        {
        options_.setOnModellingCondition( notifier);
        return this;
        }

      public Builder onResolverCondition( String notifier)
        {
        options_.setOnResolverCondition( notifier);
        return this;
        }

      public Builder enforceReadOnly()
        {
        options_.setReadOnlyEnforced( true);
        return this;
        }

      public Builder enforceWriteOnly()
        {
        options_.setWriteOnlyEnforced( true);
        return this;
        }

      public Builder maxTries( int maxTries)
        {
        options_.setMaxTries( maxTries);
        return this;
        }

      public Builder random( Long seed)
        {
        options_.setRandomSeed( seed);
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

    File inputDir =
      apiSpecFile==null
      ? options.getWorkingDir()
      : apiSpecFile.getParentFile();
    
    // Generate requested input definition
    logger_.info( "Reading API spec from {}", apiSpecFile==null? "standard input" : apiSpecFile);
    logger_.info( "Generating an input model based on API {}", options.getModelOptions().getSource().equals( ModelOptions.Source.EXAMPLES)? "examples" : "schemas");
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
        boolean requestTestCases = options.isTests() && options.isRequestCases();
        outputFile =
          new File(
            String.format(
              "%s%s-%s-%s.json",
              Optional.ofNullable( apiSpecFile.getParent()).map( p -> p + "/").orElse( ""),
              getBaseName( apiSpecFile.getName()),
              requestTestCases? "Request" : options.isServerTest()? "Requests" : "Responses",
              requestTestCases? "Cases" : options.isTests()? "Test" : "Input"));
        }
      if( outputFile != null)
        {
        // Ensure output directory exists.
        if( outputDir == null)
          {
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
      AbstractFilter transformer;
      if( !(options.isTests() && options.getTransformType() != null))
        {
        transformer = null;
        }
      else if( options.getTransformType() == Options.TransformType.JUNIT)
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
      else
        {
        transformer =
          Optional.ofNullable( options.getTransformDef())
          .map( transformDefFile -> !transformDefFile.isAbsolute()? new File( inputDir, transformDefFile.getPath()) : transformDefFile)
          .map( transformDefFile -> new TransformFilter( transformDefFile, options.getTransformParams()))
          .orElse( null);
        }
      if( transformer != null)
        {
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
        throw new IllegalStateException( "Can't open output file=" + outputFile, e);
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
      else if( options.isRequestCases())
        {
        if( options.getRandomSeed() == null)
          {
          options.setRandomSeed( options.getDefaultRandomSeed());
          }
        logger_.info( "Generating request test cases using random seed={}", options.getRandomSeed());
        TcasesOpenApiIO.writeRequestCases( Tcases.getTests( inputDef, null, null), options.getResolverContext(), outputStream);
        }
      else
        {
        TcasesOpenApiIO.writeTests( Tcases.getTests( inputDef, null, null), outputStream);
        }
      }
    }

  private static final Logger logger_ = LoggerFactory.getLogger( ApiCommand.class);
  }
