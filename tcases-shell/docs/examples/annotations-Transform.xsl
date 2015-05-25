<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <!-- 
         //////////////////////////////////////////////////////////////////////////////
         // 
         //                    Copyright 2015, Cornutum Project
         //                             www.cornutum.org
         //
         //////////////////////////////////////////////////////////////////////////////
    -->

    <xsl:output method="text"/>

    <xsl:template match="TestCases">
        <xsl:text>public class </xsl:text><xsl:value-of select="@system"/><xsl:text> {
</xsl:text>
        <xsl:for-each select="Function">
            <xsl:variable name="function" select="@name"/>

            <xsl:for-each select="TestCase">
                <xsl:text>
  @Test
  public void test_</xsl:text> <xsl:value-of select="$function"/> <xsl:text>_</xsl:text> <xsl:value-of select="@id"/> <xsl:text>()</xsl:text> <xsl:text> {
</xsl:text>

                <xsl:variable name="pageName" select="Has[@name='pageName']/@value"/>
                <xsl:text>    </xsl:text>
                <xsl:value-of select="Has[@name='pageType']/@value"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="$pageName"/>
                <xsl:text> = </xsl:text>
                <xsl:value-of select="Has[@name='pageValue']/@value"/>
                <xsl:text>;

</xsl:text>
                <xsl:variable name="shapeName" select="Input/Var[@name='Type']/Has[@name='varName']/@value"/>
                <xsl:for-each select="Input/Var">
                    <xsl:text>    </xsl:text>
                    <xsl:value-of select="Has[@name='varType']/@value"/>
                    <xsl:text> </xsl:text>
                    <xsl:value-of select="Has[@name='varName']/@value"/>
                    <xsl:text> = </xsl:text>
                    <xsl:choose>
                        <xsl:when test="Has[@name='varEval']">
                            <xsl:value-of select="Has[@name='varEval']/@value"/>
                            <xsl:text>(</xsl:text>
                            <xsl:value-of select="@value"/>
                            <xsl:text>)</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:choose>
                                <xsl:when test="Has[@name='varType']/@value='String'">
                                    <xsl:text>"</xsl:text><xsl:value-of select="@value"/><xsl:text>"</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="@value"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:otherwise>
                        
                    </xsl:choose>
                    <xsl:text>;
</xsl:text>
                    <xsl:if test="Has[@name='varApply']">
                        <xsl:text>    </xsl:text>
                        <xsl:value-of select="$shapeName"/>
                        <xsl:text>.</xsl:text>
                        <xsl:value-of select="Has[@name='varApply']/@value"/>
                        <xsl:text>(</xsl:text>
                        <xsl:value-of select="Has[@name='varName']/@value"/>
                        <xsl:text>);
                        </xsl:text>
                    </xsl:if>
                    <xsl:text>
</xsl:text>
                </xsl:for-each> 

                <xsl:text>    </xsl:text>
                <xsl:value-of select="$pageName"/>
                <xsl:text>.</xsl:text>
                <xsl:value-of select="$function"/>
                <xsl:text>(</xsl:text>
                <xsl:value-of select="$shapeName"/>
                <xsl:text>);
</xsl:text>
                <xsl:text>  }
</xsl:text>
            </xsl:for-each>

        </xsl:for-each>
        <xsl:text>}
</xsl:text>
    </xsl:template>

</xsl:stylesheet>
