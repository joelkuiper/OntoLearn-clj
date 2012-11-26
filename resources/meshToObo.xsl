<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"  xmlns:func="http://drugis.org/functions" >
    <xsl:strip-space  elements="*"/>
    <xsl:key name="TreeNode" match="//DescriptorRecord/DescriptorName" use="../TreeNumberList/TreeNumber"></xsl:key>
    <xsl:key name="RecordId" match="//DescriptorRecord/DescriptorUI" use="../DescriptorName"></xsl:key>
    
    <xsl:template match="//DescriptorRecord">
        <xsl:if test="TreeNumberList/TreeNumber[contains(text(), 'C')]" >
            <xsl:text>[Term]&#10;</xsl:text>
            <xsl:text>id: </xsl:text><xsl:value-of select="DescriptorUI"></xsl:value-of>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>name: </xsl:text><xsl:value-of select="DescriptorName"></xsl:value-of>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>def: </xsl:text><xsl:value-of select="ConceptList/Concept/ScopeNote"></xsl:value-of>
            <xsl:apply-templates select="TreeNumberList/TreeNumber"></xsl:apply-templates>
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
    </xsl:template>
    
    <xsl:function name="func:strip-last">
        <xsl:param name="str"></xsl:param>
        <xsl:value-of select="substring($str, 1, string-length($str) - 1)"></xsl:value-of>
    </xsl:function>
    
    <xsl:template match="TreeNumber">
        <xsl:variable name="treeNumber" select="." />

        <xsl:variable name="parentNumber">
            <xsl:for-each select="tokenize($treeNumber, '\.')">
                <xsl:if test="position() != last()">
                    <xsl:value-of select="."></xsl:value-of>
                    <xsl:text>.</xsl:text>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:if test="string-length($parentNumber) &gt; 3">
            <xsl:variable name="parentName" select="key('TreeNode', func:strip-last($parentNumber))"></xsl:variable>
            <xsl:text>is_a: </xsl:text>
            <xsl:value-of select="key('RecordId', $parentName)"></xsl:value-of>
            <xsl:text> ! </xsl:text>
            <xsl:value-of select="$parentName"></xsl:value-of>
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
    </xsl:template>
</xsl:stylesheet>