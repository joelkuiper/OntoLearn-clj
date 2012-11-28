<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"  xmlns:local="local" exclude-result-prefixes="#all" >
    <xsl:output method="text" indent="no"/>
    <xsl:strip-space elements="*"/>
    <xsl:key name="TreeRef" match="//TreeNumberList/TreeNumber" use="../../DescriptorName"></xsl:key>
    <xsl:key name="TreeNode" match="//DescriptorRecord/DescriptorName" use="../TreeNumberList/TreeNumber"></xsl:key>
    <xsl:key name="RecordId" match="//DescriptorRecord/DescriptorUI" use="../DescriptorName"></xsl:key>
    
    <xsl:function name="local:getUniqueTreeNumbers">
        <xsl:param name="pNodes"/>
        <xsl:param name="pLocal"/>
        <xsl:variable name="vAllTreeNumbers">
            <xsl:for-each select="$pNodes">
                <xsl:analyze-string select="." regex="(.*)\.[^.]*$">
                    <xsl:matching-substring>
                        <xsl:value-of select="concat(key('TreeNode', regex-group(1), $pLocal),'::')"/>
                    </xsl:matching-substring>
                </xsl:analyze-string>
            </xsl:for-each>
        </xsl:variable>
        <xsl:sequence select="distinct-values(tokenize(normalize-space($vAllTreeNumbers),'::'))"></xsl:sequence>
    </xsl:function>
    
    <xsl:variable name="regex">^C(0[1-9]|1[0-9]|20|2[5-9])(\.[^.]+)*$</xsl:variable>
    <xsl:template match="//DescriptorRecord">
        <xsl:if test="TreeNumberList/TreeNumber[matches(text(), $regex)]" >
            <xsl:text>[Term]&#10;</xsl:text>
            <xsl:value-of select="concat('id: MSH:', DescriptorUI, '&#10;')"></xsl:value-of>
            <xsl:value-of select="concat('name: ', DescriptorName, '&#10;')"></xsl:value-of>
            <xsl:if test="count(ConceptList/Concept/ScopeNote) &gt; 0">
                <xsl:text>def: "</xsl:text>
                <xsl:value-of select="replace(normalize-space(ConceptList/Concept[1]/ScopeNote[1]), '\]|\[|\}|\{','*')"></xsl:value-of>
                <xsl:text>"&#10;</xsl:text>
                <!-- <xsl:for-each select="ConceptList/Concept/ScopeNote">
                    <xsl:value-of select="replace(normalize-space(.), '\]|\[|\}|\{','*')" />
                    <xsl:if test="not(position()=last())">
                        <xsl:text>&#10;</xsl:text>
                    </xsl:if>
                    <xsl:if test="position()=last()">
                        <xsl:text>"&#10;</xsl:text>
                    </xsl:if>
                </xsl:for-each> -->
            </xsl:if>
            <xsl:variable name="synonyms" select="ConceptList/Concept/TermList"></xsl:variable>
            <xsl:if test="count($synonyms) &gt; 0">
                <xsl:for-each select="$synonyms/Term">
                    <xsl:value-of select="concat('synonym: ', ./String, '&#10;')"></xsl:value-of>
                </xsl:for-each>
            </xsl:if>
            <xsl:variable name="localScope" select="/"></xsl:variable>
            <xsl:for-each select="local:getUniqueTreeNumbers(TreeNumberList/TreeNumber[matches(text(), $regex)], $localScope)">
                <xsl:if test="string-length(.) &gt; 1">
                    <xsl:text>is_a: MSH:</xsl:text>
                    <xsl:value-of select="key('RecordId', ., $localScope)"></xsl:value-of>
                    <xsl:value-of select="concat( ' ! ', .)"></xsl:value-of>
                    <xsl:text>&#10;</xsl:text>
                </xsl:if>
            </xsl:for-each>
            <xsl:for-each select="TreeNumberList/TreeNumber">
                <xsl:value-of select="concat('xref: MeSHTree:', ., '&#10;')"></xsl:value-of>
            </xsl:for-each>
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
    </xsl:template>
</xsl:stylesheet>


