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
    this( true, null, null);
    }
  
  /**
   * Creates a new TestDefToHtmlFilter, using the given CSS stylesheet and JavaScript resources.
   */
  public TestDefToHtmlFilter( URI stylesheet, URI script)
    {
    this( false, stylesheet, script);
    }
  
  /**
   * Creates a new TestDefToHtmlFilter. If <CODE>defaultStyle</CODE> is true, uses the default CSS stylesheet.
   * Otherwise, uses the given CSS stylesheet and JavaScript resources.
   */
  private TestDefToHtmlFilter( boolean defaultStyle, URI stylesheet, URI script)
    {
    setDefaultStyle( defaultStyle);
    setStylesheet( stylesheet);
    setScript( script);
    }

  /**
   * Changes if this filter uses the default CSS stylesheet.
   */
  public void setDefaultStyle( boolean defaultStyle)
    {
    defaultStyle_ = defaultStyle;
    }

  /**
   * Returns if this filter uses the default CSS stylesheet.
   */
  public boolean isDefaultStyle()
    {
    return defaultStyle_;
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
   * Changes the reference to the JavaScript resource used by this filter.
   */
  public void setScript( URI script)
    {
    script_ = script;
    }

  /**
   * Returns the reference to the JavaScript resource used by this filter.
   */
  public URI getScript()
    {
    return script_;
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
        isDefaultStyle(),
        getStylesheet(),
        getScript());

    htmlWriter.flush();
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "stylesheet", getStylesheet())
      .append( "script", getScript())
      .toString();
    }

  private boolean defaultStyle_;
  private URI stylesheet_;
  private URI script_;
  }
