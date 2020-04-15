//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;
import java.util.Optional;

/**
 * Defines the source for input to a {@link TestWriter}.
 */
public class TestSource
  {
  /**
   * Creates a new TestSource instance.
   */
  public TestSource( RequestTestDef testDef)
    {
    testDef_ = testDef;
    }

  /**
   * Returns the request test definition for this source.
   */
  public RequestTestDef getTestDef()
    {
    return testDef_;
    }

  /**
   * Returns the API name for this source.
   */
  public String getApi()
    {
    return getTestDef().getApi();
    }

  /**
   * Changes the request path for this source.
   */
  public void setPath( String path)
    {
    path_ = path;
    }

  /**
   * Returns the request path for this source or null if all paths are used.
   */
  public String getPath()
    {
    return path_;
    }

  /**
   * Changes the request operation for this source.
   */
  public void setOperation( String operation)
    {
    op_ = operation;
    }

  /**
   * Returns the request operation for this source or null if all operations are used.
   */
  public String getOperation()
    {
    return op_;
    }

  /**
   * Returns the request test cases for this source.
   */
  public List<RequestCase> getRequestCases()
    {
    return getTestDef().getRequestCases( getPath(), getOperation());
    }

  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);

    builder.append( getTestDef());
    Optional.ofNullable( getOperation()).ifPresent( op -> builder.append( op));
    Optional.ofNullable( getPath()).ifPresent( p -> builder.append( p));
    
    return builder.toString();    
    }

  /**
   * Returns a new {@link Builder}.
   */
  public static Builder from( RequestTestDef testDef)
    {
    return new Builder( testDef);
    }
  
  private RequestTestDef testDef_;
  private String path_;
  private String op_;

  /**
   * Builds a {@link TestSource} instance.
   */
  public static class Builder
    {
    /**
     * Creates a new {@link Builder}
     */
    private Builder( RequestTestDef testDef)
      {
      source_ = new TestSource( testDef);
      }

    public Builder path( String path)
      {
      source_.setPath( path);
      return this;
      }

    public Builder operation( String operation)
      {
      source_.setOperation( operation);
      return this;
      }
    
    /**
     * Returns the {@link TestSource} instance for this builder.
     */
    public TestSource build()
      {
      return source_;
      }

    private TestSource source_;
    }
  }
