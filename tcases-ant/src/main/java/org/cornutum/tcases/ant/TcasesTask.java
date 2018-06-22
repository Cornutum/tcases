//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.ant;

import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.Tcases.Options;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.Parameter;

import java.io.File;

/**
 * Defines an Ant task for {@link Tcases}. The properties of this class define the attributes of the task.
 * For example, the {@link #setInputDef setInputDef} method defines a task attribute named "inputDef".
 * For a detailed description of these properties, see {@link Options Tcases.Options}. 
 * <P/>
 * To define a transform parameter (see the {@link Options -p option}), use a nested <CODE>&lt;param&gt;</CODE> element.
 *
 */
public class TcasesTask extends Task
  {
  /**
   * Runs {@link Tcases} with the specified options.
   */
  public void execute() throws BuildException
    {
    try
      {
      log( "Generating test cases for inputDef=" + options_.getInputDef());

      File logFile = getLogFile();
      if( logFile != null)
        {
        System.setProperty( "tcases.log.file", logFile.getAbsolutePath());
        log( "For details, see " + logFile.getAbsolutePath());
        }

      Tcases.run( options_);
      }
    catch( Exception e)
      {
      throw new BuildException( e, getLocation());
      }
    }

  /**
   * Changes the system input definition path.
   */
  public void setInputDef( File inputDef)
    {
    options_.setInputDef( inputDef);
    }

  /**
   * Returns the system input definition path.
   */
  public File getInputDef()
    {
    return options_.getInputDef();
    }

  /**
   * Changes the default tuple size for all generators.
   */
  public void setTuples( Integer tuples)
    {
    options_.setDefaultTupleSize( tuples);
    }

  /**
   * Returns the default tuple size for all generators.
   */
  public Integer getTuples()
    {
    return options_.getDefaultTupleSize();
    }

  /**
   * Changes the test definition output path.
   */
  public void setOutFile( File outFile)
    {
    options_.setOutFile( outFile);
    }

  /**
   * Returns the test definition output path.
   */
  public File getOutFile()
    {
    return options_.getOutFile();
    }

  /**
   * Changes the generator definition path.
   */
  public void setGenDef( File genDef)
    {
    options_.setGenDef( genDef);
    }

  /**
   * Returns the generator definition path.
   */
  public File getGenDef()
    {
    return options_.getGenDef();
    }

  /**
   * Changes if using the JUnit transform.
   */
  public void setJunit( boolean junit)
    {
    if( junit)
      {
      options_.setTransformType( Options.TransformType.JUNIT);
      }
    }

  /**
   * Returns if using the JUnit transform.
   */
  public boolean isJunit()
    {
    return options_.getTransformType() == Options.TransformType.JUNIT;
    }

  /**
   * Changes if using the HTML transform.
   */
  public void setHtml( boolean html)
    {
    if( html)
      {
      options_.setTransformType( Options.TransformType.HTML);
      }
    }

  /**
   * Returns if using the HTML transform.
   */
  public boolean isHtml()
    {
    return options_.getTransformType() == Options.TransformType.HTML;
    }

  /**
   * Changes the log output file for Tcases.
   */
  public void setLogFile( File logFile)
    {
    logFile_ = logFile;
    }

  /**
   * Returns the log output file for Tcases.
   */
  public File getLogFile()
    {
    return logFile_;
    }

  /**
   * Changes if previous contents of the test definition file are ignored.
   * If false, new test definitions are based on the previous test definitions.
   */
  public void setNew( boolean newDef)
    {
    options_.setExtended( !newDef);
    }

  /**
   * Returns if previous contents of the test definition file are ignored.
   * If false, new test definitions are based on the previous test definitions.
   */
  public boolean isNew()
    {
    return !options_.isExtended();
    }

  /**
   * Changes the output directory for generated test definitions.
   */
  public void setOutDir( File outDir)
    {
    options_.setOutDir( outDir);
    }

  /**
   * Returns the output directory for generated test definitions.
   */
  public File getOutDir()
    {
    return options_.getOutDir();
    }

  /**
   * Adds a transform parameter.
   */
  public void addConfiguredParam( Parameter param)
    {
    options_.getTransformParams().put( param.getName(), param.getValue());
    }

  /**
   * Changes the random seed used by generators.
   */
  public void setSeed( Long seed)
    {
    options_.setRandomSeed( seed);
    }

  /**
   * Returns the random seed used by generators.
   */
  public Long getSeed()
    {
    return options_.getRandomSeed();
    }

  /**
   * If true, automatically chooses a new random seed used by generators.
   */
  public void setNewSeed( boolean newSeed)
    {
    options_.setNewSeed( newSeed);
    }

  /**
   * Returns if automatically choosing a new random seed used by generators.
   */
  public boolean isNewSeed()
    {
    return options_.isNewSeed();
    }

  /**
   * Changes the transform file.
   */
  public void setTransformDef( File transformDef)
    {
    options_.setTransformDef( transformDef);
    if( transformDef != null)
      {
      options_.setTransformType( Options.TransformType.CUSTOM);
      }
    }

  /**
   * Returns the transform file.
   */
  public File getTransformDef()
    {
    return options_.getTransformDef();
    }

  /**
   * Changes the output path for generated test definitions.
   */
  public void setTestDef( File testDef)
    {
    options_.setTestDef( testDef);
    }

  /**
   * Returns the output path for generated test definitions.
   */
  public File getTestDef()
    {
    return options_.getTestDef();
    }

  private Options options_ = new Options();
  private File logFile_;
  }

