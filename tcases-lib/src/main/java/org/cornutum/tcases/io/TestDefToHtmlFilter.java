//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.util.ToString;

import java.net.URI;

/**
 * A {@link AbstractFilter filter} that transforms a system test definition document
 * into an HTML report.
 */
public class TestDefToHtmlFilter extends AbstractFilter
  {
  /**
   * Creates a new TestDefToHtmlFilter, using the default CSS stylesheet.
   */
  public TestDefToHtmlFilter()
    {
    this( true, null);
    }
  
  /**
   * Creates a new TestDefToHtmlFilter, using the given CSS stylesheet.
   */
  public TestDefToHtmlFilter( URI stylesheet)
    {
    this( stylesheet != null, stylesheet);
    }
  
  /**
   * Creates a new TestDefToHtmlFilter, using the given CSS stylesheet.
   */
  private TestDefToHtmlFilter( boolean styled, URI stylesheet)
    {
    setStyled( styled);
    setStylesheet( stylesheet);
    }

  /**
   * Changes if this filter uses a CSS stylesheet.
   */
  public void setStyled( boolean styled)
    {
    styled_ = styled;
    }

  /**
   * Returns if this filter uses a CSS stylesheet.
   */
  public boolean isStyled()
    {
    return styled_;
    }

  /**
   * Changes the reference to the CSS stylesheet used by this filter.
   */
  public void setStylesheet( URI stylesheet)
    {
    stylesheet_ = stylesheet;
    }

  /**
   * Returns the reference to the CSS stylesheet used by this filter.
   */
  public URI getStylesheet()
    {
    return stylesheet_;
    }

  /**
   * Reads data to be transformed from the {@link #getFilterInput filter input stream} and
   * write transformed data to the {@link #getFilterOutput filter output stream}.
   */
  @SuppressWarnings("resource")
  protected void applyFilter() throws Exception
    {
    SystemTestHtmlWriter htmlWriter = new SystemTestHtmlWriter( getFilterOutput());

    htmlWriter.write
      ( new SystemTestDocReader( getFilterInput()).getSystemTestDef(),
        isStyled(),
        getStylesheet());

    htmlWriter.flush();
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "stylesheet", getStylesheet())
      .toString();
    }

  private boolean styled_;
  private URI stylesheet_;
  }
