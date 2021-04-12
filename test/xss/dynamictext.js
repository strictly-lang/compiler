describe("host element handling", () => {
    let container;
    beforeEach(() => {
        container = document.createElement("div");
        document.body.appendChild(container);
    });
    afterEach(() => {
        container.remove();
    });

    it("dynamic text should not interpret html, but just show it as a string", () => {
        const element = document.createElement("test-components-text-dynamic-index");
        element.foo = "<div />";
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[0].textContent).toBe("con-<div />-cat");
        expect(element.shadowRoot.childNodes[0].querySelector("div")).toBe(null);
        element.foo = "<span />";

        expect(element.shadowRoot.childNodes[0].textContent).toBe("con-<span />-cat");
        expect(element.shadowRoot.childNodes[0].querySelector("span")).toBe(null);
    });
});
