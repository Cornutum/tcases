//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.generator.io.*;
import org.cornutum.tcases.io.*;
import static org.cornutum.tcases.io.Resource.withDefaultType;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static org.apache.commons.lang3.ObjectUtils.firstNonNull;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}.
 *
 */
public class TcasesCommand
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
   * [-r <I>seed</I>] [-R]
   * [-t <I>testDef</I>]
   * [-T <I>contentType</I>]
   * [-v]
   * [-x <I>transformDef</I> | -J | -H]
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
   * The default generator definition is read from the corresponding <CODE>*-Generators</CODE> file in the same directory as the <I>inputDef</I>,
   * if it exists. Otherwise, the default {@link TupleGenerator} is used for all functions.
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
   * <TR valign="baseline">
   * <TD><B> values </B></TD>
   * <TD>
   * A boolean value (true/false, yes/no). If true, comments listing all input variable assignments
   * are included in the body of each test method.
   * If omitted, the default is true.
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
   * <NOBR>-R </NOBR>
   * </TD>
   * <TD>
   * Choose a new random number seed for all generators. This updates the generator definitions specified by the
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
   * <NOBR>-v </NOBR>
   * </TD>
   * <TD>
   * Prints the current Tcases version identifier to standard output.
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
    public enum TransformType { HTML, JUNIT, CUSTOM };

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

      else if( arg.equals( "-R"))
        {
        setNewSeed( true);
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
        new IllegalArgumentException
        ( "Usage: "
          + Tcases.class.getSimpleName()
          + " [-v]"
          + " [-c tupleSize]"
          + " [-f outFile]"
          + " [-g genDef]"
          + " [-n]"
          + " [-o outDir]"
          + " [-p name=value]"
          + " [-r seed] [-R]"
          + " [-t testDef]"
          + " [-T contentType]"
          + " [-x transformDef | -J | -H]"
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
      getGeneratorOptions().setRandomSeed( seed);
      }

    /**
     * Returns the random seed used by generators.
     */
    public Long getRandomSeed()
      {
      if( getGeneratorOptions().getRandomSeed() == null && isNewSeed())
        {
        setRandomSeed( (long) (Math.random() * Long.MAX_VALUE));
        }

      return getGeneratorOptions().getRandomSeed();
      }

    /**
     * Changes if choosing a new random seed used by generators.
     */
    public void setNewSeed( boolean newSeed)
      {
      newSeed_ = newSeed;
      }

    /**
     * Returns if choosing a new random seed used by generators.
     */
    public boolean isNewSeed()
      {
      return newSeed_;
      }

    /**
     * Changes the default tuple size used by generators.
     */
    public void setDefaultTupleSize( Integer tupleSize)
      {
      getGeneratorOptions().setDefaultTupleSize( tupleSize);
      }

    /**
     * Returns the default tuple size used by generators.
     */
    public Integer getDefaultTupleSize()
      {
      return getGeneratorOptions().getDefaultTupleSize();
      }

    /**
     * Returns the options used by generators.
     */
    public GeneratorOptions getGeneratorOptions()
      {
      return generatorOptions_;
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

      if( showVersion())
        {
        builder.append( " -v");
        }

      if( getTransformDef() != null)
        {
        builder.append( " -x ").append( getTransformDef().getPath());
        }

      if( getTransformType() == TransformType.JUNIT)
        {
        builder.append( " -J");
        }

      if( getTransformType() == TransformType.HTML)
        {
        builder.append( " -H");
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
    private TransformType transformType_;
    private boolean extended_;
    private boolean newSeed_;
    private File workingDir_;
    private boolean showVersion_;
    private GeneratorOptions generatorOptions_ = new GeneratorOptions();
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

      public Builder transformDef( File transformDef)
        {
        options_.setTransformDef( transformDef);
        return this;
        }

      public Builder transformType( TransformType transformType)
        {
        options_.setTransformType( transformType);
        return this;
        }

      public Builder transformParam( String name, String value)
        {
        options_.getTransformParams().put( name, value);
        return this;
        }

      public Builder newSeed()
        {
        options_.setNewSeed( true);
        return this;
        }

      public Builder extended()
        {
        options_.setExtended( true);
        return this;
        }

      public Builder seed( Long seed)
        {
        options_.setRandomSeed( seed);
        return this;
        }

      public Builder tuples( int tuples)
        {
        options_.setDefaultTupleSize( tuples);
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
   * Creates a new Tcases object.
   */
  private TcasesCommand()
    {
    // Static methods only
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
   * Generates a set of {@link TestCase test cases} from a {@link SystemInputDef system input definition}
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

    File inputDir =
      inputDefFile==null
      ? options.getWorkingDir()
      : inputDefFile.getParentFile();

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

    // Test definition defined?
    String projectName = getProjectName( inputDefFile);
    File testDefFile = options.getTestDef();
    if( testDefFile == null && inputDefFile != null)
      {
      // No, derive default from input file.
      testDefFile = withDefaultType( new File( projectName + "-Test"), defaultContentType);
      }

    // Identify base test definition file.
    File baseDefFile = testDefFile;
    if( baseDefFile != null && !baseDefFile.isAbsolute())
      {
      // For relative path, read base test definitions from input directory.
      baseDefFile = new File( inputDir, baseDefFile.getPath());
      }

    // Output file defined?
    File outputDir = options.getOutDir();
    File outputFile = options.getOutFile();
    if ( outputFile == null)
      {
      // No, defaults to...
      outputFile =
        // ... JUnit test class file, if generating JUnit
        options.getTransformType() == Options.TransformType.JUNIT && inputDefFile != null
        ? new File( projectName.replaceAll( "\\W+", "") + "Test.java") :

        // ... HTML file, if generating HTML
        options.getTransformType() == Options.TransformType.HTML && inputDefFile != null
        ? new File( projectName + "-Test.htm") :

        // ... else test definition file.
        testDefFile;
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

      outputFile =
        new File
        ( outputDir,
          outputFile.isAbsolute()? outputFile.getName() : outputFile.getPath());
      }

    Resource.Type outputFileType = firstNonNull( Resource.Type.of( withDefaultType( outputFile, defaultContentType)), defaultContentType);

    SystemTestDef baseDef = null;
    if( options.isExtended() && baseDefFile != null && baseDefFile.exists())
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
      genDefDefault = withDefaultType( new File( inputDir, projectName + "-Generators"), defaultContentType);
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
      logger_.info( "Reading generator definition={}", genDefFile);
      try( GeneratorSetResource reader = withDefaultType( GeneratorSetResource.of( genDefFile), defaultContentType))
        {
        genDef = reader.getGeneratorSet();
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't read generator definition file=" + genDefFile, e);
        }
      }
    else
      {
      // No, use default TupleGenerator.
      genDef = GeneratorSet.basicGenerator();
      }

    // Generate new test definitions.
    SystemTestDef testDef = Tcases.getTests( inputDef, genDef, baseDef, options.getGeneratorOptions());

    // Identify test definition transformations.
    AbstractFilter transformer = null;
    File transformDefFile = options.getTransformDef();
    if( transformDefFile != null)
      {
      if( !transformDefFile.isAbsolute())
        {
        transformDefFile = new File( inputDir, transformDefFile.getPath());
        }
      transformer = new TransformFilter( transformDefFile, options.getTransformParams());
      }
    else if( options.getTransformType() == Options.TransformType.JUNIT)
      {
      transformer = new TestDefToJUnitFilter( options.getTransformParams());
      }
    else if( options.getTransformType() == Options.TransformType.HTML)
      {
      transformer = new TestDefToHtmlFilter();
      }

    if( transformer != null)
      {
      if( outputFile != null && outputFile.equals( baseDefFile) && baseDefFile.exists())
        {
        throw new RuntimeException( "Transformed output will overwrite test definition file=" + baseDefFile);
        }
      transformer.setTarget( outputFile);

      // Input to transformers must be XML
      outputFileType = Resource.Type.XML;
      }

    // Write new test definitions.
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

      if( outputFileType == Resource.Type.JSON)
        {
        TcasesJson.writeTests( testDef, output);
        }
      else
        {
        TcasesIO.writeTests( testDef, output);
        }
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write test definition file=" + outputFile, e);
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
      logger_.info( "Updating generator definition={}", genUpdateFile);
      try
        {
        Resource.Type genUpdateType = firstNonNull( Resource.Type.of( withDefaultType( genUpdateFile, defaultContentType)), defaultContentType);
        if( genUpdateType == Resource.Type.JSON)
          {
          TcasesJson.writeGenerators( genDef, new FileOutputStream( genUpdateFile));
          }
        else
          {
          TcasesIO.writeGenerators( genDef, new FileOutputStream( genUpdateFile));
          }
        }
      catch( Exception e)
        {
        throw new RuntimeException( "Can't write generator definition file=" + genUpdateFile, e);
        }
      }
    }

  /**
   * Returns the name of the project for the given input definition file.
   */
  public static String getProjectName( File inputDefFile)
    {
    String projectName = null;
    if( inputDefFile != null)
      {
      String inputBase = FilenameUtils.getBaseName( inputDefFile.getName());

      projectName =
        inputBase.toLowerCase().endsWith( "-input")
        ? inputBase.substring( 0, inputBase.length() - "-input".length())
        : inputBase;
      }

    return projectName;
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
      "Tcases "
      + tcasesProperties.getProperty( "tcases.version") + " ("
      + tcasesProperties.getProperty( "tcases.date")
      + ")";
    }

  private static final Logger logger_ = LoggerFactory.getLogger( Tcases.class);
  }
