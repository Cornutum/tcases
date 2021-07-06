//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;

/**
 * A composite matcher for {@link AuthDef} instances
 */
public class AuthDefMatcher extends BaseMatcher<AuthDef>
  {
  /**
   * Creates a new AuthDefMatcher instance.
   */
  public AuthDefMatcher( AuthDef expected)
    {
    delegate_ = MatcherFactory.matcherFor( expected);
    }

  public boolean matches( Object item)
    {
    return delegate_.matches( item);
    }

  public void describeMismatch( Object item, Description mismatchDescription)
    {
    delegate_.describeMismatch( item, mismatchDescription);
    }

  public void describeTo( Description mismatchDescription)
    {
    delegate_.describeTo( mismatchDescription);
    }

  public String toString()
    {
    return delegate_.toString();
    }

  private final Matcher<?> delegate_;

  /**
   * Creates a type-specific Matcher for an {@link AuthDef}.
   */
  private static class MatcherFactory implements AuthDefVisitor
    {
    /**
     * Creates a new MatcherFactory instance.
     */
    public static Matcher<?> matcherFor( AuthDef expected)
      {
        MatcherFactory factory = new MatcherFactory();
        expected.accept( factory);
        return factory.matcher_;
      }
    
    public void visit( ApiKeyDef authDef)
      {
      matcher_ = new ApiKeyDefMatcher( authDef);
      }
    
    public void visit( HttpBasicDef authDef)
      {
      matcher_ = new HttpBasicDefMatcher( authDef);
      }
    
    public void visit( HttpBearerDef authDef)
      {
      matcher_ = new HttpBearerDefMatcher( authDef);
      }

    private Matcher<?> matcher_;
    }

  }
