//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.util.ToString;

import static org.apache.commons.lang3.StringUtils.abbreviateMiddle;
import static org.apache.commons.lang3.StringUtils.trimToNull;

import java.io.File;
import java.util.List;
import java.util.Optional;
import static java.util.stream.Collectors.joining;

/**
 * Defines the target for output from a {@link JavaTestWriter}.
 */
public class JavaTestTarget extends TestTarget
  {
  /**
   * Creates a new JavaTestTarget instance.
   */
  public JavaTestTarget()
    {
    }

  /**
   * Changes the Java package defined for this target.
   */
  public void setPackage( String packageName)
    {
    package_ = packageName;
    }

  /**
   * Changes the Java package defined for this target to the package containing the given class.
   */
  public void setPackage( Class<?> packageMember)
    {
    setPackage(
      Optional.ofNullable( packageMember)
      .map( Class::getPackage)
      .map( Package::getName)
      .orElse( null));
    }

  /**
   * Returns the Java package defined for this target.
   */
  public String getPackage()
    {
    return package_;
    }

  /**
   * Changes the fully-qualified name of the base class defined for this target.
   */
  public void setBaseClass( String baseClassName)
    {
    baseClass_ = baseClassName;
    }

  /**
   * Changes the fully-qualified name of the base class defined for this target.
   */
  public void setBaseClass( Class<?> baseClass)
    {
    setBaseClass( baseClass == null? null : baseClass.getName());
    }

  /**
   * Returns the fully-qualified name of the base class defined for this target,
   * or null if no base class is defined.
   */
  public String getBaseClass()
    {
    return baseClass_;
    }

  /**
   * Returns the simple name of the base class defined for this target,
   * or null if no base class is defined.
   */
  public String getBaseClassName()
    {
    return
      Optional.ofNullable( getBaseClass())
      .map( fqn -> fqn.lastIndexOf( "."))
      .filter( packageEnd -> packageEnd >= 0)
      .map( packageEnd -> getBaseClass().substring( packageEnd + 1))
      .orElse( getBaseClass());
    }

  /**
   * Returns the package name for the base class defined for this target,
   * or null if no base class is defined.
   */
  public String getBaseClassPackage()
    {
    return
      Optional.ofNullable( getBaseClass())
      .map( fqn -> fqn.lastIndexOf( "."))
      .filter( packageEnd -> packageEnd >= 0)
      .map( packageEnd -> getBaseClass().substring( 0, packageEnd))
      .orElse( null);
    }

  /**
   * Returns the Java package for this target.
   */
  public String getTargetPackage()
    {
    return
      Optional.ofNullable( getPackage())
      .orElseGet( this::getDefaultPackage);
    }

  /**
   * Returns the default Java package for this target.
   */
  private String getDefaultPackage()
    {
    File targetDir =
      Optional.ofNullable( getDir())
      .orElse(
        Optional.ofNullable( getFile())
        .map( File::getParentFile)
        .orElse( null));

    return
      Optional.ofNullable( targetDir)
      .map( File::getAbsoluteFile)
      .flatMap( this::getMavenPackage)
      .orElse( null);
    }

  /**
   * If the given directory belongs to a Maven project, returns the corresponding Java package.
   */
  private Optional<String> getMavenPackage( File dir)
    {
    List<String> dirs = TestTarget.getPathElements( dir);
    int root = dirs.indexOf( "java");
    return
      root >= 0
      ? Optional.ofNullable( trimToNull( dirs.subList( root + 1, dirs.size()).stream().collect( joining( "."))))
      : Optional.empty();
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "package", abbreviateMiddle( getPackage(), "...", 20))
      .appendSuper( super.toString())
      .toString();
    }

  /**
   * Returns a new {@link Builder}.
   */
  public static Builder builder()
    {
    return new Builder();
    }
  
  private String package_;
  private String baseClass_;

  /**
   * Builds a {@link TestTarget} instance.
   */
  public static class Builder extends BaseBuilder<Builder>
    {
    /**
     * Creates a new {@link Builder}
     */
    private Builder()
      {
      target_ = new JavaTestTarget();
      }

    /**
     * Returns the {@link TestTarget} instance for this builder.
     */
    @Override
    protected TestTarget getTestTarget()
      {
      return target_;
      }

    public Builder inPackage( Class<?> packageMember)
      {
      target_.setPackage( packageMember);
      return this;
      }

    public Builder inPackage( String packageName)
      {
      target_.setPackage( packageName);
      return this;
      }

    public Builder extending( String baseClass)
      {
      target_.setBaseClass( baseClass);
      return this;
      }

    public Builder extending( Class<?> baseClass)
      {
      target_.setBaseClass( baseClass);
      return this;
      }
    
    /**
     * Returns the {@link JavaTestTarget} instance for this builder.
     */
    public JavaTestTarget build()
      {
      return target_;
      }

    private JavaTestTarget target_;
    }
  }
