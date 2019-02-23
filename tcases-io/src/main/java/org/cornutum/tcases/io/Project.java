//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.generator.IGeneratorSet;
import org.cornutum.tcases.generator.io.GeneratorSetException;
import org.cornutum.tcases.generator.io.GeneratorSetResource;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;

/**
 * Represents the collection of resources used to generate test cases for a specific project.
 */
public class Project
  {
  /**
   * Creates a new Project instance.
   */
  public Project()
    {
    }

  /**
   * Changes the system input definition for this project.
   */
  public void setSystemInput( SystemInputDef systemInputDef)
    {
    systemInputDef_ = systemInputDef;

    if( systemInputDef != null)
      {
      setSystemInputLocation( null);
      }
    }

  /**
   * Returns the system input definition for this project.
   */
  public SystemInputDef getSystemInput()
    {
    if( systemInputDef_ == null && systemInputRef_ != null)
      {
      try( SystemInputResource resource = SystemInputResource.at( urlFor( systemInputRef_)))
        {
        systemInputDef_ = resource.getSystemInputDef();
        }
      catch( Exception e)
        {
        throw new SystemInputException( String.format( "Can't read resource at %s", systemInputRef_), e);
        }
      }
    
    return systemInputDef_;
    }

  /**
   * If no system input location is defined for this project, returns the system input definition directly referenced by this project.
   */
  SystemInputDef getSystemInputValue()
    {
    return systemInputDef_;
    }

  /**
   * Changes the location of the system input definition for this project.
   */
  public void setSystemInputLocation( URI location)
    {
    systemInputRef_ = location;

    if( location != null)
      {
      setSystemInput( null);
      }
    }

  /**
   * Returns the location of the system input definition for this project.
   */
  public URI getSystemInputLocation()
    {
    return systemInputRef_;
    }

  /**
   * Changes the generator set for this project.
   */
  public void setGenerators( IGeneratorSet generatorSet)
    {
    generatorSet_ = generatorSet;

    if( generatorSet != null)
      {
      setGeneratorsLocation( null);
      }
    }

  /**
   * Returns the generator set for this project.
   */
  public IGeneratorSet getGenerators() {
    if( generatorSet_ == null && generatorSetRef_ != null)
      {
      try( GeneratorSetResource resource = GeneratorSetResource.at( urlFor( generatorSetRef_)))
        {
        generatorSet_ = resource.getGeneratorSet();
        }
      catch( Exception e)
        {
        throw new GeneratorSetException( String.format( "Can't read resource at %s", generatorSetRef_), e);
        }
      }
    return generatorSet_;
    }

  /**
   * If no generator set location is defined for this project, returns the generator set directly referenced by this project.
   */
  IGeneratorSet getGeneratorsValue()
    {
    return generatorSet_;
    }

  /**
   * Changes the location of the generator set for this project.
   */
  public void setGeneratorsLocation( URI location)
    {
    generatorSetRef_ = location;

    if( location != null)
      {
      setGenerators( null);
      }
    }

  /**
   * Returns the location of the generator set for this project.
   */
  public URI getGeneratorsLocation()
    {
    return generatorSetRef_;
    }

  /**
   * Changes the base tests definition for this project.
   */
  public void setBaseTests( SystemTestDef systemTestDef)
    {
    baseTestDef_ = systemTestDef;

    if( systemTestDef != null)
      {
      setBaseTestsLocation( null);
      }
    }

  /**
   * Returns the base tests definition for this project.
   */
  public SystemTestDef getBaseTests() {
    if( baseTestDef_ == null && baseTestRef_ != null)
      {
      try( SystemTestResource resource = SystemTestResource.at( urlFor( baseTestRef_)))
        {
        baseTestDef_ = resource.getSystemTestDef();
        }
      catch( Exception e)
        {
        throw new SystemTestException( String.format( "Can't read resource at %s", baseTestRef_), e);
        }
      }
    return baseTestDef_;
    }



  /**
   * If no base test location is defined for this project, returns the base test definition directly referenced by this project.
   */
  SystemTestDef getBaseTestsValue() {
    return baseTestDef_;
    }

  /**
   * Changes the location of the base tests definition for this project.
   */
  public void setBaseTestsLocation( URI location)
    {
    baseTestRef_ = location;

    if( location != null)
      {
      setBaseTests( null);
      }
    }

  /**
   * Returns the location of the base tests definition for this project.
   */
  public URI getBaseTestsLocation()
    {
    return baseTestRef_;
    }

  /**
   * Changes the base URI for the location of project elements.
   */
  public void setBaseLocation( URI location)
    {
    refBase_ = location;
    }

  /**
   * Returns the base URI for the location of project elements.
   */
  public URI getBaseLocation()
    {
    return refBase_;
    }

  /**
   * Returns the full URL for the project element at the given location.
   */
  public URL urlFor( URI location) throws MalformedURLException
    {
    URI uri =
      getBaseLocation() == null
      ? location
      : getBaseLocation().resolve( location);

    return uri.toURL();
    }

  private SystemInputDef systemInputDef_;
  private IGeneratorSet generatorSet_;
  private SystemTestDef baseTestDef_;
  private URI refBase_;
  private URI systemInputRef_;
  private URI generatorSetRef_;
  private URI baseTestRef_;
  }
