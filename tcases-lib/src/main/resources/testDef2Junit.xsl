<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <!-- 
  //////////////////////////////////////////////////////////////////////////////
  // 
  //                    Copyright 2012, Cornutum Project
  //                             www.cornutum.org
  //
  //////////////////////////////////////////////////////////////////////////////
  -->
  <!-- Transforms system test definitions into JUnit test source code -->
  <!-- $Revision$, $Date$ -->

  <xsl:output method="text"/>

  <xsl:variable name="lowercase" select="'abcdefghijklmnopqrstuvwxyz'"/> 
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/> 

  <xsl:param name="throws" select="'false'"/>
  <xsl:variable name="throwsValue" select="translate($throws,$uppercase,$lowercase)"/>
  <xsl:param name="system" select=""/>
  <xsl:param name="class" select=""/>
  <xsl:param name="values" select="'true'"/>
  <xsl:variable name="valuesValue" select="translate($values,$uppercase,$lowercase)"/>

  <xsl:template match="TestCases">
    <xsl:variable name="systemId" select="@system"/>

    <xsl:for-each select="Function">
      <xsl:variable name="function" select="@name"/>

      <xsl:for-each select="TestCase">
        <xsl:variable name="id" select="@id"/>
        <xsl:variable name="failureCase" select="translate(@failure,$uppercase,$lowercase)"/>

        <xsl:text>
  /**
   * Tests </xsl:text>
        <xsl:choose>
          <xsl:when test="$system">
            <xsl:value-of select="$system"/>
          </xsl:when>
          <xsl:when test="$class">
            <xsl:text>{@link </xsl:text>
            <xsl:value-of select="$class"/>
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$function"/>
            <xsl:text> </xsl:text>
            <xsl:value-of select="$function"/>
            <xsl:text>()}</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>{@link </xsl:text>
            <xsl:value-of select="$systemId"/>
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$function"/>
            <xsl:text> </xsl:text>
            <xsl:value-of select="$function"/>
            <xsl:text>()}</xsl:text>
          </xsl:otherwise>
        </xsl:choose>        
        <xsl:text> using the following inputs.
   * &lt;P&gt;
   * &lt;TABLE border="1" cellpadding="8"&gt;
   * &lt;TR align="left"&gt;&lt;TH colspan=2&gt; </xsl:text>
        <xsl:value-of select="$id"/>
        <xsl:text>. </xsl:text>
        <xsl:value-of select="$function"/>
        <xsl:text> (</xsl:text>
        <xsl:choose>
          <xsl:when test="$failureCase='true' or $failureCase='yes'">
            <xsl:text>&lt;FONT color="red"&gt;Failure&lt;/FONT&gt;</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>Success</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>) &lt;/TH&gt;&lt;/TR&gt;
   * &lt;TR align="left"&gt;&lt;TH&gt; Input Choice &lt;/TH&gt; &lt;TH&gt; Value &lt;/TH&gt;&lt;/TR&gt;
</xsl:text>

        <xsl:for-each select="Input/Var">
          <xsl:variable name="failureValue" select="translate(@failure,$uppercase,$lowercase)"/>
          <xsl:text>   * &lt;TR&gt;&lt;TD&gt; </xsl:text>
          <xsl:value-of select="@name"/>
          <xsl:text> &lt;/TD&gt; &lt;TD&gt; </xsl:text>
          <xsl:choose>
            <xsl:when test="$failureValue='true' or $failureValue='yes'">
              <xsl:text>&lt;FONT color="red"&gt; </xsl:text>
              <xsl:value-of select="@value"/>
              <xsl:text>  &lt;/FONT&gt;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@value"/>
            </xsl:otherwise>
          </xsl:choose> 
          <xsl:text> &lt;/TD&gt; &lt;/TR&gt;
</xsl:text>
        </xsl:for-each>

        <xsl:text>   * &lt;/TABLE&gt;
   * &lt;/P&gt;
   */
  @Test
  public void </xsl:text>
        <xsl:value-of select="$function"/>
        <xsl:text>_</xsl:text>
        <xsl:value-of select="$id"/>
        <xsl:text>()</xsl:text>
        <xsl:if test="$throwsValue='true' or $throwsValue='yes'">
          <xsl:text> throws Exception</xsl:text>
        </xsl:if>
        <xsl:text>
    {</xsl:text>
        <xsl:if test="count(Has) > 0">
          <xsl:for-each select="Has">
            <xsl:text>
    // </xsl:text> <xsl:value-of select="@name"/> <xsl:text> = </xsl:text> <xsl:value-of select="@value"/>
          </xsl:for-each>
          <xsl:text>
</xsl:text> 
        </xsl:if>
        <xsl:text>
    // Given...</xsl:text>
        <xsl:if test="count(Input/Var/Has) > 0 or $valuesValue='true' or $valuesValue='yes'">
          <xsl:for-each select="Input/Var">
              <xsl:text>
    //</xsl:text>
              <xsl:text>
    //   </xsl:text> <xsl:value-of select="@name"/> <xsl:text> = </xsl:text> <xsl:value-of select="@value"/>
              <xsl:for-each select="Has">
                  <xsl:text>
    //     </xsl:text> <xsl:value-of select="@name"/> <xsl:text> = </xsl:text> <xsl:value-of select="@value"/>
              </xsl:for-each>
          </xsl:for-each> 
        </xsl:if>
        <xsl:text>
    
    // When...

    // Then...
    }
</xsl:text>
      </xsl:for-each>

    </xsl:for-each>

  </xsl:template>

</xsl:stylesheet>
